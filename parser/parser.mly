%{
  open Ast
%}

/* 标记声明 */
%token <Z.t> INT_LIT
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
%start <program> program

%%

program:
  | fns = func_def EOF { fns }
  ;

func_def:
  | FN IDENT LPAREN RPAREN body = block
    { body }
  ;

block:
  | LBRACE stmts = list(statement) RBRACE { stmts }
  ;

statement:
  | LET name = IDENT
    init = option(preceded(ASSIGN, expr)) SEMICOLON
    { SLet(name, init) }
  | target = IDENT ASSIGN value = expr SEMICOLON
    { SAssign(target, value) }
  | e = expr SEMICOLON
    { SExpr(e) }
  | RETURN SEMICOLON
    { SReturn }
  | b = block
    { SBlock(b) }
  | IF cond = expr then_branch = statement %prec THEN
    { SIf(cond, then_branch, None) }
  | IF cond = expr then_branch = statement ELSE else_branch = statement
    { SIf(cond, then_branch, Some else_branch) }
  | WHILE cond = expr body = statement
    { SWhile(cond, body) }
  ;

expr:
  | id = IDENT { EVariable(id) }
  | lit = literal { ELiteral(lit) }
  | LPAREN e = expr RPAREN { e }
  | e1 = expr PLUS e2 = expr { EBinaryOp(e1, BAdd, e2) }
  | e1 = expr MINUS e2 = expr { EBinaryOp(e1, BSub, e2) }
  | e1 = expr STAR e2 = expr { EBinaryOp(e1, BMul, e2) }
  | e1 = expr SLASH e2 = expr { EBinaryOp(e1, BDiv, e2) }
  | e1 = expr PERCENT e2 = expr { EBinaryOp(e1, BMod, e2) }
  | e1 = expr EQ e2 = expr { EBinaryOp(e1, BEq, e2) }
  | e1 = expr NEQ e2 = expr { EBinaryOp(e1, BNe, e2) }
  | e1 = expr LT e2 = expr { EBinaryOp(e1, BLt, e2) }
  | e1 = expr GT e2 = expr { EBinaryOp(e1, BGt, e2) }
  | e1 = expr LEQ e2 = expr { EBinaryOp(e1, BLe, e2) }
  | e1 = expr GEQ e2 = expr { EBinaryOp(e1, BGe, e2) }
  | e1 = expr AND e2 = expr { EBinaryOp(e1, BAnd, e2) }
  | e1 = expr OR e2 = expr { EBinaryOp(e1, BOr, e2) }
  | NOT e = expr %prec UNOT { EUnaryOp(UNot, e) }
  | MINUS e = expr %prec UMINUS { EUnaryOp(UNeg, e) }
  ;

/* 字面量表达式 */
literal:
  | b = INT_LIT { b }
  ;