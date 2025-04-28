open Core
open Ast
open Parser.DCST

let unparse_literial = integer_literal

let option_expr =
  Option.fold ~init:(none_preceded_assign_expr_ ()) ~f:(fun _ ->
      some_preceded_assign_expr_ )

(* let list_statement = function
  | [] -> none_separated_nonempty_list_semicolon_statement_ ()
  | hd :: tl ->
      tl
      |> List.fold_right ~f:more_semicolon_statement
           ~init:(one_semicolon_statement hd)
      |> some_separated_nonempty_list_semicolon_statement_ *)

let rec unparse_expr =
  let choice e = expr_choice e (paren e) in
  function
  | EUnit -> unit ()
  | EVariable v -> variable v
  | ELiteral l -> unparse_literial l |> literal
  | EAssign (v, e) ->
      let e = unparse_expr e in
      assign v e
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
  | EReturn e -> unparse_expr e |> return_expr
  | (EIf _ | EWhile _ | EBlock _) as e ->
      unparse_standalone_expr e |> Option.value_exn |> standalone_expr
      |> choice

and unparse_standalone_expr = function
  | EIf (cond, then_branch, else_branch) ->
      Some
        (let cond = unparse_expr cond in
         let then_branch = unparse_block then_branch in
         let else_branch = Option.map ~f:unparse_block else_branch in
         match else_branch with
         | None -> if_expr cond then_branch
         | Some else_branch -> if_else_expr cond then_branch else_branch )
  | EWhile (cond, body) ->
      let cond = unparse_expr cond in
      let body = unparse_block body in
      Some (while_expr cond body)
  | EBlock _ as e -> Some (unparse_block e |> block_expr)
  | _ -> None

and unparse_statement_list = function
  | [] -> failwith "empty statement list"
  | s :: [] -> (
    match s with
    | SLet _ as s ->
        unparse_statement_item s |> Option.value_exn |> item_stmt_list
    | SExpr e -> unparse_expr e |> expr_stmt_list
    | SSemi e -> unparse_expr e |> semi_stmt_list )
  | s :: ss -> (
    match s with
    | SLet _ as s ->
        item_cons_stmt_list
          (unparse_statement_item s |> Option.value_exn)
          (unparse_statement_list ss)
    | SExpr e -> (
      match unparse_standalone_expr e with
      | Some e -> expr_cons_stmt_list e (unparse_statement_list ss)
      | None ->
          semi_cons_stmt_list (unparse_expr e) (unparse_statement_list ss) )
    | SSemi e ->
        semi_cons_stmt_list (unparse_expr e) (unparse_statement_list ss) )

and unparse_statement_item = function
  | SLet (v, e) ->
      let e = e |> Option.map ~f:unparse_expr |> option_expr in
      Some (let_stmt v e)
  | _ -> None

and unparse_block = function
  | EBlock s -> failwith "TODO"
  | e -> unparse_expr e |> expr_stmt_list |> block

let unparse_program = function
  | PProgram (name, p) -> p |> unparse_block |> func_def name |> program

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
      let s = self#visit_statement_list b in
      sym "{" ^^ (break 1 ^^ s |> nest 2) ^^ break 1 ^^ sym "}" |> group

    method! visit_statement_item s = super#visit_statement_item s |> group

    method! case_func_def name body =
      let name = self#visit_IDENT name in
      let body = self#visit_block body in
      (kwd "fn " ^^ name ^^ sym "() " |> group) ^^ body

    method! case_if_expr cond then_branch =
      let cond = self#visit_expr cond in
      let then_branch = self#visit_block then_branch in
      (kwd "if " ^^ cond |> group) ^^ break 1 ^^ then_branch

    method! case_if_else_expr cond then_branch else_branch =
      let cond = self#visit_expr cond in
      let then_branch = self#visit_block then_branch in
      let else_branch = self#visit_block else_branch in
      (kwd "if " ^^ cond |> group)
      ^^ (break 1 ^^ then_branch |> nest 2)
      ^^ break 1 ^^ kwd "else"
      ^^ (break 1 ^^ else_branch |> nest 2)
      |> group

    method! case_while_expr cond body =
      let cond = self#visit_expr cond in
      let body = self#visit_block body in
      (kwd "while " ^^ cond |> group) ^^ break 1 ^^ body
  end

let unparse p =
  let printer = new printer in
  let buf = Buffer.create 16 in
  match unparse_program p |> Parser.Settle.program with
  | None -> failwith "unparse failed"
  | Some program ->
      printer#visit_program program |> PPrint.ToBuffer.pretty 0.8 100 buf ;
      Buffer.contents buf
