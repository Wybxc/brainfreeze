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
%token ASSIGN PLUS_ASSIGN MINUS_ASSIGN STAR_ASSIGN SLASH_ASSIGN PERCENT_ASSIGN
%token AND OR NOT BIT_AND BIT_OR BIT_XOR BIT_NOT SHL SHR
%token EOF

/* 非终结符的类型声明 */
%type <Ast.func_def> func_def
%type <Ast.func_def list> list(func_def)
%type <Ast.param> parameter
%type <Ast.param list> separated_list(COMMA, parameter)
%type <Ast.param list> separated_nonempty_list(COMMA,parameter)
%type <Ast.typ> type_expr
%type <Ast.typ> return_type
%type <Ast.typ option> option(preceded(COLON,type_expr))
%type <int> array_size
%type <int option> option(array_size)
%type <Ast.stmt list> block
%type <Ast.stmt list> list(statement)
%type <Ast.stmt> statement
%type <Ast.expr> expr
%type <Ast.expr> primary_expr
%type <Ast.expr> postfix_expr
%type <Ast.expr> prefix_expr
%type <Ast.expr> mult_expr
%type <Ast.expr> add_expr
%type <Ast.expr> shift_expr
%type <Ast.expr> rel_expr
%type <Ast.expr> eq_expr
%type <Ast.expr> bit_and_expr
%type <Ast.expr> bit_xor_expr
%type <Ast.expr> bit_or_expr
%type <Ast.expr> log_and_expr
%type <Ast.expr> log_or_expr
%type <Ast.expr> assign_expr
%type <Ast.expr list> separated_list(COMMA, expr)
%type <Ast.expr list> separated_nonempty_list(COMMA,expr)
%type <Ast.expr option> option(expr)
%type <Ast.expr option> option(preceded(ASSIGN,expr))
%type <Ast.literal> literal
%type <Ast.literal> byte_array
%type <int list> byte_list

/* 优先级和结合性声明 - 只保留必要的声明 */
%nonassoc THEN
%nonassoc ELSE
%right NEG

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
  | target = expr PLUS_ASSIGN value = expr SEMICOLON
    { Assign(target, BinOp(target, Add, value)) }
  | target = expr MINUS_ASSIGN value = expr SEMICOLON
    { Assign(target, BinOp(target, Sub, value)) }
  | target = expr STAR_ASSIGN value = expr SEMICOLON
    { Assign(target, BinOp(target, Mul, value)) }
  | target = expr SLASH_ASSIGN value = expr SEMICOLON
    { Assign(target, BinOp(target, Div, value)) }
  | target = expr PERCENT_ASSIGN value = expr SEMICOLON
    { Assign(target, BinOp(target, Mod, value)) }
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
  | e = assign_expr { e }
  ;

assign_expr:
  | e = log_or_expr { e }
  | lhs = postfix_expr ASSIGN rhs = assign_expr          { BinOp(lhs, Eq, rhs) }
  | lhs = postfix_expr PLUS_ASSIGN rhs = assign_expr     { BinOp(lhs, Add, rhs) }
  | lhs = postfix_expr MINUS_ASSIGN rhs = assign_expr    { BinOp(lhs, Sub, rhs) }
  | lhs = postfix_expr STAR_ASSIGN rhs = assign_expr     { BinOp(lhs, Mul, rhs) }
  | lhs = postfix_expr SLASH_ASSIGN rhs = assign_expr    { BinOp(lhs, Div, rhs) }
  | lhs = postfix_expr PERCENT_ASSIGN rhs = assign_expr  { BinOp(lhs, Mod, rhs) }
  ;

log_or_expr:
  | e = log_and_expr { e }
  | e1 = log_or_expr OR e2 = log_and_expr { BinOp(e1, Or, e2) }
  ;

log_and_expr:
  | e = bit_or_expr { e }
  | e1 = log_and_expr AND e2 = bit_or_expr { BinOp(e1, And, e2) }
  ;

bit_or_expr:
  | e = bit_xor_expr { e }
  | e1 = bit_or_expr BIT_OR e2 = bit_xor_expr { BinOp(e1, BitOr, e2) }
  ;

bit_xor_expr:
  | e = bit_and_expr { e }
  | e1 = bit_xor_expr BIT_XOR e2 = bit_and_expr { BinOp(e1, BitXor, e2) }
  ;

bit_and_expr:
  | e = eq_expr { e }
  | e1 = bit_and_expr BIT_AND e2 = eq_expr { BinOp(e1, BitAnd, e2) }
  ;

eq_expr:
  | e = rel_expr { e }
  | e1 = eq_expr EQ e2 = rel_expr { BinOp(e1, Eq, e2) }
  | e1 = eq_expr NEQ e2 = rel_expr { BinOp(e1, Neq, e2) }
  ;

rel_expr:
  | e = shift_expr { e }
  | e1 = rel_expr LT e2 = shift_expr { BinOp(e1, Lt, e2) }
  | e1 = rel_expr GT e2 = shift_expr { BinOp(e1, Gt, e2) }
  | e1 = rel_expr LEQ e2 = shift_expr { BinOp(e1, Leq, e2) }
  | e1 = rel_expr GEQ e2 = shift_expr { BinOp(e1, Geq, e2) }
  ;

shift_expr:
  | e = add_expr { e }
  | e1 = shift_expr SHL e2 = add_expr { BinOp(e1, Shl, e2) }
  | e1 = shift_expr SHR e2 = add_expr { BinOp(e1, Shr, e2) }
  ;

add_expr:
  | e = mult_expr { e }
  | e1 = add_expr PLUS e2 = mult_expr { BinOp(e1, Add, e2) }
  | e1 = add_expr MINUS e2 = mult_expr { BinOp(e1, Sub, e2) }
  ;

mult_expr:
  | e = prefix_expr { e }
  | e1 = mult_expr STAR e2 = prefix_expr { BinOp(e1, Mul, e2) }
  | e1 = mult_expr SLASH e2 = prefix_expr { BinOp(e1, Div, e2) }
  | e1 = mult_expr PERCENT e2 = prefix_expr { BinOp(e1, Mod, e2) }
  ;

prefix_expr:
  | e = postfix_expr { e }
  | MINUS e = prefix_expr %prec NEG { UnaryOp(Neg, e) }
  | NOT e = prefix_expr { UnaryOp(Not, e) }
  | BIT_NOT e = prefix_expr { UnaryOp(BitNot, e) }
  ;

postfix_expr:
  | e = primary_expr { e }
  | func = IDENT LPAREN args = separated_list(COMMA, expr) RPAREN
    { Call(func, args) }
  | arr = postfix_expr LBRACKET idx = expr RBRACKET
    { Index(arr, idx) }
  ;

primary_expr:
  | lit = literal { Literal(lit) }
  | id = IDENT { Variable(id) }
  | LPAREN e = expr RPAREN { e }
  ;

literal:
  | b = BYTE_LIT { LByte(b) }
  | s = STRING_LIT { LString(s) }
  | bytes = byte_array { bytes }
  ;

byte_array:
  | LBRACKET bytes = byte_list RBRACKET { LByteArray(bytes) }
  ;

byte_list:
  | { [] }  /* 空列表 */
  | b = BYTE_LIT { [b] }
  | b = BYTE_LIT COMMA rest = byte_list { b :: rest }
  ;