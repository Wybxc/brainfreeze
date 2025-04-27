(* Parser驱动程序 *)
open Lexing

(* 错误处理 *)
exception Syntax_error of string

(* 位置信息辅助函数 *)
let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

(* 位置信息格式化函数，返回字符串 *)
let string_of_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

(* 解析函数 *)
let parse_string s =
  let lexbuf = Lexing.from_string s in
  try
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "string" };
    Parser.program Lexer.token lexbuf
  with
  | Parser.Error ->
      raise (Syntax_error (Printf.sprintf "At %s, syntax error" (string_of_position lexbuf)))
  | Failure msg ->
      raise (Syntax_error (Printf.sprintf "At %s, %s" (string_of_position lexbuf) msg))

(* 从文件解析 *)
let parse_file filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  try
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    let result = Parser.program Lexer.token lexbuf in
    close_in chan;
    result
  with
  | e ->
      close_in chan;
      raise e

(* 解析并格式化 *)
let parse_and_format s =
  let ast = parse_string s in
  Unparser.string_of_program ast

(* 测试函数 *)
let test_parse s =
  try
    let ast = parse_string s in
    let formatted = Unparser.string_of_program ast in
    Printf.printf "解析成功!\n原始代码:\n%s\n\n格式化后:\n%s\n" s formatted;
    Some ast
  with
  | Syntax_error msg ->
      Printf.printf "解析错误: %s\n" msg;
      None