{
open Parser (* 导入Parser模块生成的标记类型 *)
open Lexing

(* 跟踪位置信息的辅助函数 *)
let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = pos.pos_cnum;
              pos_lnum = pos.pos_lnum + 1
    }
}

(* 定义正则表达式别名 *)
let digit = ['0'-'9']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let alpha = ['a'-'z' 'A'-'Z']
let ident = (alpha | '_') (alpha | digit | '_')*
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let byte = "0x" hex hex?
let integer = digit+

rule token = parse
  | whitespace    { token lexbuf }
  | newline       { next_line lexbuf; token lexbuf }
  | "//"          { single_line_comment lexbuf }
  | "/*"          { multi_line_comment lexbuf }
  | byte as b     { BYTE_LIT(int_of_string b) }
  | integer as i  { INT_LIT(int_of_string i) }
  | '"'           { read_string (Buffer.create 16) lexbuf }
  | "fn"          { FN }
  | "let"         { LET }
  | "if"          { IF }
  | "else"        { ELSE }
  | "while"       { WHILE }
  | "for"         { FOR }
  | "in"          { IN }
  | "return"      { RETURN }
  | "byte"        { BYTE_T }
  | "["           { LBRACKET }
  | "]"           { RBRACKET }
  | "{"           { LBRACE }
  | "}"           { RBRACE }
  | "("           { LPAREN }
  | ")"           { RPAREN }
  | ";"           { SEMICOLON }
  | ":"           { COLON }
  | ","           { COMMA }
  | "+"           { PLUS }
  | "-"           { MINUS }
  | "*"           { STAR }
  | "/"           { SLASH }
  | "%"           { PERCENT }
  | "=="          { EQ }
  | "!="          { NEQ }
  | "<"           { LT }
  | ">"           { GT }
  | "<="          { LEQ }
  | ">="          { GEQ }
  | "="           { ASSIGN }
  | "+="          { PLUS_ASSIGN }
  | "-="          { MINUS_ASSIGN }
  | "*="          { STAR_ASSIGN }
  | "/="          { SLASH_ASSIGN }
  | "%="          { PERCENT_ASSIGN }
  | "&&"          { AND }
  | "||"          { OR }
  | "!"           { NOT }
  | "&"           { BIT_AND }
  | "|"           { BIT_OR }
  | "^"           { BIT_XOR }
  | "~"           { BIT_NOT }
  | "<<"          { SHL }
  | ">>"          { SHR }
  | ident as id   { IDENT(id) }
  | eof           { EOF }
  | _             { raise (Failure ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

(* 处理单行注释 *)
and single_line_comment = parse
  | newline       { next_line lexbuf; token lexbuf }
  | eof           { EOF }
  | _             { single_line_comment lexbuf }

(* 处理多行注释 *)
and multi_line_comment = parse
  | "*/"          { token lexbuf }
  | newline       { next_line lexbuf; multi_line_comment lexbuf }
  | eof           { raise (Failure "Unterminated comment") }
  | _             { multi_line_comment lexbuf }

(* 处理字符串字面量 *)
and read_string buf = parse
  | '"'           { STRING_LIT(Buffer.contents buf) }
  | '\\' 'n'      { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'      { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'      { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' '\\'     { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' '"'      { Buffer.add_char buf '"'; read_string buf lexbuf }
  | newline       { next_line lexbuf; Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | eof           { raise (Failure "Unterminated string") }
  | _ as c        { Buffer.add_char buf c; read_string buf lexbuf }