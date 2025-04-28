(* Parser驱动程序 *)
open Core
open Lexing
module I = Parser.MenhirInterpreter

let print_position outx lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  fprintf outx "[%s:%d:%d]" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let rec parse lexbuf (checkpoint : Ast.program I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env ->
      let token = Lexer.token lexbuf in
      let startp = lexbuf.lex_start_p and endp = lexbuf.lex_curr_p in
      let checkpoint = I.offer checkpoint (token, startp, endp) in
      parse lexbuf checkpoint
  | I.Shifting _ | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      parse lexbuf checkpoint
  | I.HandlingError env ->
      let got = lexeme lexbuf in
      let state = I.current_state_number env in
      let message =
        try
          let message = Syntax_messages.message state |> String.strip in
          sprintf "Syntax error (E%d): %s, got '%s'" state message got
        with _ -> sprintf "Syntax error (E%d): unexpected '%s'" state got
      in
      failwith message
  | I.Accepted v -> v
  | I.Rejected -> failwith "invalid syntax (parser rejected the input)"

let parse_string s =
  let lexbuf = Lexing.from_string s in
  try Parser.Incremental.program lexbuf.lex_curr_p |> parse lexbuf with
  | Parser.Error ->
      fprintf stderr "%a Unknown error\n" print_position lexbuf ;
      exit 1
  | Failure s ->
      fprintf stderr "%a %s\n" print_position lexbuf s ;
      exit 1

(* 解析并格式化 *)
let parse_and_format s =
  let ast = parse_string s in
  Unparser.unparse ast
