open Core
open Ast
open Parser.DCST

let unparse_literial = integer_literal

let unparse_option_expr =
  Option.fold ~init:(none_preceded_assign_expr_ ()) ~f:(fun _ ->
      some_preceded_assign_expr_ )

let unparse_list_statement =
  List.fold_right ~f:cons_statement ~init:(nil_statement ())

let rec unparse_expr =
  let choice e = expr_choice e (paren e) in
  function
  | EVariable v -> variable v
  | ELiteral l -> unparse_literial l |> literal
  | EUnaryOp (op, e) ->
      let e = unparse_expr e in
      let e = match op with UNeg -> negation e | UNot -> logical_not e in
      choice e
  | EBinaryOp (e1, op, e2) ->
      let l = unparse_expr e1 and r = unparse_expr e2 in
      let e =
        match op with
        | BAdd -> add l r
        | BSub -> subtract l r
        | BMul -> multiply l r
        | BDiv -> divide l r
        | BMod -> modulo l r
        | BEq -> equal l r
        | BNe -> not_equal l r
        | BLt -> less_than l r
        | BGt -> greater_than l r
        | BLe -> less_equal l r
        | BGe -> greater_equal l r
        | BAnd -> logical_and l r
        | BOr -> logical_or l r
      in
      choice e
  | EIf (cond, then_branch, else_branch) ->
      let cond = unparse_expr cond in
      let then_branch = unparse_expr_block then_branch in
      let else_branch = unparse_expr_block else_branch in
      if_else_expr cond then_branch else_branch |> choice
  | EBlock _ as e -> unparse_expr_block e |> block_expr

and unparse_statement = function
  | SLet (v, e) ->
      let e =
        e |> Option.map ~f:(fun e -> unparse_expr e) |> unparse_option_expr
      in
      let_statement v e
  | SAssign (v, e) ->
      let e = unparse_expr e in
      assignment v e
  | SExpr e ->
      let e = unparse_expr e in
      expression_statement e
  | SReturn -> return_statement ()
  | SBlock b ->
      b
      |> List.map ~f:unparse_statement
      |> unparse_list_statement |> block |> block_statement
  | SIf (cond, then_branch, else_branch) -> (
      let cond = unparse_expr cond
      and then_branch = unparse_statement_block then_branch
      and else_branch = Option.map ~f:unparse_statement_block else_branch in
      match else_branch with
      | None -> if_statement cond then_branch
      | Some else_branch -> if_else_statement cond then_branch else_branch )
  | SWhile (cond, body) ->
      let cond = unparse_expr cond and body = unparse_statement_block body in
      while_statement cond body

and unparse_expr_block = function
  | EBlock (s, e) ->
      let s = List.map ~f:unparse_statement s |> unparse_list_statement in
      let e = unparse_expr e in
      expr_block s e
  | e -> expr_block (nil_statement ()) (unparse_expr e)

and unparse_statement_block = function
  | SBlock b ->
      let s = List.map ~f:unparse_statement b |> unparse_list_statement in
      block s
  | s -> block (cons_statement (unparse_statement s) (nil_statement ()))

let unparse_program p =
  p |> unparse_statement_block |> function_definition "main" |> program

module Ansi = struct
  (** Ansi terminal colors. *)

  open Core

  let make_color c = Printf.sprintf "\x1b[%dm%s\x1b[0m" c

  let black = make_color 30

  let red = make_color 31

  let green = make_color 32

  let yellow = make_color 33

  let blue = make_color 34

  let magenta = make_color 35

  let cyan = make_color 36

  let white = make_color 37

  let bright_black = make_color 90

  let bright_red = make_color 91

  let bright_green = make_color 92

  let bright_yellow = make_color 93

  let bright_blue = make_color 94

  let bright_magenta = make_color 95

  let bright_cyan = make_color 96

  let bright_white = make_color 97

  let strip_ansi s =
    let buffer = Buffer.create (String.length s) in
    let state = ref `Text in
    String.iter s ~f:(fun c ->
        let open Char in
        match !state with
        | `Text ->
            if c = '\x1b' then state := `Escape else Buffer.add_char buffer c
        | `Escape -> if c = '[' then state := `Bracket else state := `Text
        | `Bracket -> if is_digit c then state := `Digit else state := `Text
        | `Digit ->
            if is_digit c then ()
            else if c = 'm' then state := `Text
            else state := `Text ) ;
    Buffer.contents buffer
end

type palette =
  { comment: string -> string
  ; keyword: string -> string
  ; literal: string -> string
  ; string: string -> string
  ; identifier: string -> string
  ; macro: string -> string
  ; class_: string -> string
  ; operator: string -> string
  ; symbol: string -> string
  ; type_: string -> string
  ; enum: string -> string }

let default_palette =
  { comment= Ansi.bright_black
  ; keyword= Ansi.bright_blue
  ; literal= Ansi.bright_green
  ; string= Ansi.bright_cyan
  ; identifier= Ansi.bright_white
  ; macro= Ansi.bright_magenta
  ; class_= Ansi.yellow
  ; operator= Ansi.white
  ; symbol= Ansi.white
  ; type_= Ansi.yellow
  ; enum= Ansi.yellow }

let colored get_color s =
  PPrint.fancystring (get_color default_palette s) (String.length s)

(** [comment s] pretty-prints a comment. *)
let comment = colored (fun p -> p.comment)

(** [kwd s] pretty-prints a keyword. *)
let kwd = colored (fun p -> p.keyword)

(** [lit s] pretty-prints a literal. *)
let lit = colored (fun p -> p.literal)

(** [str s] pretty-prints a string. *)
let str = colored (fun p -> p.string)

(** [id s] pretty-prints an identifier. *)
let id = colored (fun p -> p.identifier)

(** [macro s] pretty-prints a macro. *)
let macro = colored (fun p -> p.macro)

(** [cls s] pretty-prints a class. *)
let cls = colored (fun p -> p.class_)

(** [op s] pretty-prints an operator. *)
let op = colored (fun p -> p.operator)

(** [sym s] pretty-prints a symbol. *)
let sym = colored (fun p -> p.symbol)

(** [ty s] pretty-prints a type. *)
let ty = colored (fun p -> p.type_)

(** [enum s] pretty-prints an enum. *)
let enum = colored (fun p -> p.enum)

class printer =
  let open PPrint in
  object (self)
    inherit [document] Parser.CST.reduce as super

    method zero = empty

    method cat a b =
      if is_empty a then b else if is_empty b then a else a ^^ break 1 ^^ b

    method text = id

    method visit_WHILE = kwd "while"

    method visit_STRING_LIT s = str "\"" ^^ str s ^^ str "\""

    method visit_STAR = op "*"

    method visit_SLASH = op "/"

    method visit_SEMICOLON = op ";"

    method visit_RPAREN = sym ")"

    method visit_RETURN = kwd "return"

    method visit_RBRACKET = sym "]"

    method visit_RBRACE = sym "}"

    method visit_PLUS = op "+"

    method visit_PERCENT = op "%"

    method visit_OR = op "||"

    method visit_NOT = op "!"

    method visit_NEQ = op "!="

    method visit_MINUS = op "-"

    method visit_LT = op "<"

    method visit_LPAREN = sym "("

    method visit_LET = kwd "let"

    method visit_LEQ = op "<="

    method visit_LBRACKET = sym "["

    method visit_LBRACE = sym "{"

    method visit_INT_LIT l = Z.to_string l |> lit

    method visit_IN = kwd "in"

    method visit_IF = kwd "if"

    method visit_IDENT = id

    method visit_GT = op ">"

    method visit_GEQ = op ">="

    method visit_FOR = kwd "for"

    method visit_FN = kwd "fn"

    method visit_EQ = op "=="

    method visit_EOF = empty

    method visit_ELSE = kwd "else"

    method visit_COMMA = op ","

    method visit_COLON = op ":"

    method visit_BYTE_T = kwd "byte"

    method visit_ASSIGN = op "="

    method visit_AND = op "&&"

    method! case_paren e =
      sym "(" ^^ (self#visit_expr e |> nest 2) ^^ sym ")" |> group

    method! case_block b =
      let s = self#visit_list_statement_ b in
      sym "{" ^^ (break 1 ^^ s |> nest 2) ^^ break 1 ^^ sym "}" |> group

    method! case_expr_block s e =
      let s = self#visit_list_statement_ s in
      let e = self#visit_expr e in
      sym "{"
      ^^ (break 1 ^^ s ^^ break 1 ^^ e |> nest 2)
      ^^ break 1 ^^ sym "}"
      |> group

    method! visit_statement s = super#visit_statement s |> group

    method! case_function_definition name body =
      let name = self#visit_IDENT name in
      let body = self#visit_block body in
      (kwd "fn " ^^ name ^^ sym "() " |> group) ^^ body

    method! case_if_statement cond then_branch =
      let cond = self#visit_expr cond in
      let then_branch = self#visit_block then_branch in
      (kwd "if " ^^ cond |> group) ^^ break 1 ^^ then_branch

    method! case_if_else_statement cond then_branch else_branch =
      let cond = self#visit_expr cond in
      let then_branch = self#visit_block then_branch in
      let else_branch = self#visit_block else_branch in
      (kwd "if " ^^ cond |> group)
      ^^ (break 1 ^^ then_branch |> nest 2)
      ^^ break 1 ^^ kwd "else"
      ^^ (break 1 ^^ else_branch |> nest 2)
      |> group
  end

let unparse p =
  let printer = new printer in
  let buf = Buffer.create 16 in
  match unparse_program p |> Parser.Settle.program with
  | None -> failwith "unparse failed"
  | Some program ->
      printer#visit_program program |> PPrint.ToBuffer.pretty 0.8 100 buf ;
      Buffer.contents buf
