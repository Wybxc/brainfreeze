(* 将AST转换回源代码的非解析器 *)
open Ast

(* 辅助函数：缩进代码 *)
let indent_string str =
  let lines = String.split_on_char '\n' str in
  let indented =
    List.map (fun line -> if line = "" then line else "  " ^ line) lines
  in
  String.concat "\n" indented

(* 将类型转换为字符串 *)
let string_of_type = function
  | TByte -> "byte"
  | TByteArray None -> "[byte]"
  | TByteArray (Some size) -> Printf.sprintf "[byte; %d]" size

(* 将字面量转换为字符串 *)
let string_of_literal = function
  | LByte b -> string_of_int b (* 使用十进制输出 *)
  | LByteArray bytes ->
      let byte_strs = List.map string_of_int bytes in
      (* 使用十进制输出 *)
      "[" ^ String.concat ", " byte_strs ^ "]"
  | LString s -> "\"" ^ String.escaped s ^ "\""

(* 将二元操作符转换为字符串 *)
let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Gt -> ">"
  | Leq -> "<="
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

(* 将一元操作符转换为字符串 *)
let string_of_unaryop = function Neg -> "-" | Not -> "!"

(* 将表达式转换为字符串 *)
let rec string_of_expr = function
  | Literal lit -> string_of_literal lit
  | Variable id -> id
  | BinOp (e1, op, e2) ->
      let s1 = string_of_expr e1 in
      let s2 = string_of_expr e2 in
      Printf.sprintf "(%s %s %s)" s1 (string_of_binop op) s2
  | UnaryOp (op, e) -> string_of_unaryop op ^ "(" ^ string_of_expr e ^ ")"
  | Call (func, args) ->
      let arg_strs = List.map string_of_expr args in
      func ^ "(" ^ String.concat ", " arg_strs ^ ")"
  | Index (arr, idx) -> string_of_expr arr ^ "[" ^ string_of_expr idx ^ "]"

(* 将语句转换为字符串 *)
let rec string_of_stmt = function
  | Let (name, typ_opt, init_opt) ->
      let typ_str =
        match typ_opt with
        | Some typ -> ": " ^ string_of_type typ
        | None -> ""
      in
      let init_str =
        match init_opt with
        | Some expr -> " = " ^ string_of_expr expr
        | None -> ""
      in
      "let " ^ name ^ typ_str ^ init_str ^ ";"
  | Assign (target, value) ->
      string_of_expr target ^ " = " ^ string_of_expr value ^ ";"
  | ExprStmt expr -> string_of_expr expr ^ ";"
  | Return None -> "return;"
  | Return (Some expr) -> "return " ^ string_of_expr expr ^ ";"
  | Block stmts ->
      let stmt_strs = List.map string_of_stmt stmts in
      "{\n" ^ indent_string (String.concat "\n" stmt_strs) ^ "\n}"
  | If (cond, then_branch, None) ->
      "if " ^ string_of_expr cond ^ " " ^ string_of_stmt then_branch
  | If (cond, then_branch, Some else_branch) ->
      "if " ^ string_of_expr cond ^ " "
      ^ string_of_stmt then_branch
      ^ " else "
      ^ string_of_stmt else_branch
  | While (cond, body) ->
      "while " ^ string_of_expr cond ^ " " ^ string_of_stmt body
  | For (var, iter, body) ->
      "for " ^ var ^ " in " ^ string_of_expr iter ^ " " ^ string_of_stmt body

(* 将参数转换为字符串 *)
let string_of_param (param : param) =
  param.name ^ ": " ^ string_of_type param.typ

(* 将函数定义转换为字符串 *)
let string_of_func_def func =
  let param_strs = List.map string_of_param func.params in
  let ret_type_str =
    match func.return_type with
    | Some typ -> ": " ^ string_of_type typ
    | None -> ""
  in
  "fn " ^ func.name ^ "("
  ^ String.concat ", " param_strs
  ^ ")" ^ ret_type_str ^ " " ^ string_of_stmt func.body

(* 将整个程序转换为字符串 *)
let string_of_program prog =
  let func_strs = List.map string_of_func_def prog in
  String.concat "\n\n" func_strs
