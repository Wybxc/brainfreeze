(* 类型定义 *)
type typ =
  | TByte
  (* 单个字节 *)
  | TByteArray of int option (* 字节数组，可能有指定长度 *)

(* 字面量 *)
type literal =
  | LByte of int (* 字节字面量，如 0xFF *)
  | LByteArray of int list (* 字节数组字面量，如 [0x01, 0x02, 0x03] *)
  | LString of string (* 字符串字面量，如 "hello" *)

(* 表达式 *)
type expr =
  | Literal of literal (* 字面量 *)
  | Variable of string (* 变量引用 *)
  | BinOp of expr * bin_op * expr (* 二元操作 *)
  | UnaryOp of unary_op * expr (* 一元操作 *)
  | Call of string * expr list (* 函数调用 *)
  | Index of expr * expr (* 数组索引 *)

(* 二元操作符 *)
and bin_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod (* 算术操作符 *)
  | Eq
  | Neq
  | Lt
  | Gt
  | Leq
  | Geq (* 比较操作符 *)
  | And
  | Or (* 逻辑操作符 *)

(* 一元操作符 *)
and unary_op = Neg | Not (* 负号，逻辑非 *)

(* 语句 *)
type stmt =
  | Let of string * typ option * expr option (* 变量声明，可能有类型和初始值 *)
  | Assign of expr * expr (* 赋值语句 *)
  | ExprStmt of expr (* 表达式语句 *)
  | Return of expr option (* 返回语句 *)
  | Block of stmt list (* 语句块 *)
  | If of expr * stmt * stmt option (* if语句，可能有else *)
  | While of expr * stmt (* while循环 *)
  | For of string * expr * stmt (* for循环 (简化版) *)

(* 函数参数 *)
type param = {name: string; typ: typ}

(* 函数定义 *)
type func_def =
  {name: string; params: param list; return_type: typ option; body: stmt}

(* 完整程序由函数定义组成 *)
type program = func_def list
