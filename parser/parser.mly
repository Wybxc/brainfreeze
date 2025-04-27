%{
  open Ast
%}

/* 标记声明 */
%token <int> BYTE_LIT INT_LIT
%token <string> IDENT STRING_LIT
%token BYTE_T FN LET IF ELSE WHILE FOR IN RETURN
%token LBRACKET RBRACKET LBRACE RBRACE LPAREN RPAREN
%token SEMICOLON COLON COMMA
%token PLUS MINUS STAR SLASH PERCENT
%token EQ NEQ LT GT LEQ GEQ
%token ASSIGN
%token AND OR NOT
%token EOF

/* 添加操作符优先级和结合性规则 */
%nonassoc THEN
%nonassoc ELSE

%left OR
%left AND
%nonassoc EQ NEQ LT GT LEQ GEQ
%left PLUS MINUS
%left STAR SLASH PERCENT
%right UNOT
%right UMINUS

/* 入口点 */
%start <Ast.program> program

%%

program:
  | fns = list(func_def) EOF { fns }
  ;

func_def:
  | FN name = IDENT LPAREN params = separated_list(COMMA, parameter) RPAREN
    ret_type = option(return_type) body = block
    { { name; params; return_type = ret_type; body = Block body } }
  ;

parameter:
  | name = IDENT COLON typ = type_expr { { name; typ } }
  ;

return_type:
  | COLON typ = type_expr { typ }
  ;

type_expr:
  | BYTE_T { TByte }
  | LBRACKET BYTE_T opt_size = option(array_size) RBRACKET { TByteArray opt_size }
  ;

array_size:
  | SEMICOLON size = INT_LIT { size }
  ;

block:
  | LBRACE stmts = list(statement) RBRACE { stmts }
  ;

statement:
  | LET name = IDENT
    typ = option(preceded(COLON, type_expr))
    init = option(preceded(ASSIGN, expr)) SEMICOLON
    { Let(name, typ, init) }
  | target = expr ASSIGN value = expr SEMICOLON
    { Assign(target, value) }
  | e = expr SEMICOLON
    { ExprStmt(e) }
  | RETURN e = option(expr) SEMICOLON
    { Return(e) }
  | b = block
    { Block(b) }
  | IF cond = expr then_branch = statement %prec THEN
    { If(cond, then_branch, None) }
  | IF cond = expr then_branch = statement ELSE else_branch = statement
    { If(cond, then_branch, Some else_branch) }
  | WHILE cond = expr body = statement
    { While(cond, body) }
  | FOR var = IDENT IN iterable = expr body = statement
    { For(var, iterable, body) }
  ;

expr:
  | id = IDENT { Variable(id) }
  | lit = literal { Literal(lit) }
  | LPAREN e = expr RPAREN { e }
  | e1 = expr PLUS e2 = expr { BinOp(e1, Add, e2) }
  | e1 = expr MINUS e2 = expr { BinOp(e1, Sub, e2) }
  | e1 = expr STAR e2 = expr { BinOp(e1, Mul, e2) }
  | e1 = expr SLASH e2 = expr { BinOp(e1, Div, e2) }
  | e1 = expr PERCENT e2 = expr { BinOp(e1, Mod, e2) }
  | e1 = expr EQ e2 = expr { BinOp(e1, Eq, e2) }
  | e1 = expr NEQ e2 = expr { BinOp(e1, Neq, e2) }
  | e1 = expr LT e2 = expr { BinOp(e1, Lt, e2) }
  | e1 = expr GT e2 = expr { BinOp(e1, Gt, e2) }
  | e1 = expr LEQ e2 = expr { BinOp(e1, Leq, e2) }
  | e1 = expr GEQ e2 = expr { BinOp(e1, Geq, e2) }
  | e1 = expr AND e2 = expr { BinOp(e1, And, e2) }
  | e1 = expr OR e2 = expr { BinOp(e1, Or, e2) }
  | NOT e = expr %prec UNOT { UnaryOp(Not, e) }
  | MINUS e = expr %prec UMINUS { UnaryOp(Neg, e) }
  ;

/* 字面量表达式 */
literal:
  | b = BYTE_LIT { LByte(b) }
  | s = STRING_LIT { LString(s) }
  | LBRACKET bytes = byte_list RBRACKET { LByteArray(bytes) }
  ;

/* 字节列表 */
byte_list:
  | { [] }  /* 空列表 */
  | b = BYTE_LIT { [b] }
  | b = BYTE_LIT COMMA rest = byte_list { b :: rest }
  | i = INT_LIT {
      if i < 0 || i > 255 then
        raise (Failure (Printf.sprintf "Integer %d is out of byte range (0-255)" i))
      else [i]
    }
  | i = INT_LIT COMMA rest = byte_list {
      if i < 0 || i > 255 then
        raise (Failure (Printf.sprintf "Integer %d is out of byte range (0-255)" i))
      else i :: rest
    }
  ;