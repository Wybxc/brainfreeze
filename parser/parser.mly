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
  | fns = func_def EOF { fns } [@name program]
  ;

func_def:
  | FN IDENT LPAREN RPAREN body = block
    { body } [@name function_definition]
  ;

block:
  | LBRACE stmts = list(statement) RBRACE { SBlock(stmts) } [@name block]
  ;

statement:
  | LET name = IDENT
    init = option(preceded(ASSIGN, expr)) SEMICOLON
    { SLet(name, init) } [@name let_statement]
  | target = IDENT ASSIGN value = expr SEMICOLON
    { SAssign(target, value) } [@name assignment]
  | e = expr SEMICOLON
    { SExpr(e) } [@name expression_statement]
  | RETURN SEMICOLON
    { SReturn } [@name return_statement]
  | b = block
    { b } [@name block_statement]
  | IF cond = expr then_branch = block %prec THEN
    { SIf(cond, then_branch, None) } [@name if_statement]
  | IF cond = expr then_branch = block ELSE else_branch = block
    { SIf(cond, then_branch, Some else_branch) } [@name if_else_statement]
  | WHILE cond = expr body = block
    { SWhile(cond, body) } [@name while_statement]
  ;

block_expr:
  | LBRACE stmts = list(statement) e = expr RBRACE
    { EBlock(stmts, e) } [@name expr_block]

expr:
  | id = IDENT { EVariable(id) } [@name variable]
  | lit = literal { ELiteral(lit) } [@name literal]
  | LPAREN e = expr RPAREN { e } [@name paren]
  | e = block_expr { e } [@name block_expr]
  | e1 = expr PLUS e2 = expr { EBinaryOp(e1, BAdd, e2) } [@name add]
  | e1 = expr MINUS e2 = expr { EBinaryOp(e1, BSub, e2) } [@name subtract]
  | e1 = expr STAR e2 = expr { EBinaryOp(e1, BMul, e2) } [@name multiply]
  | e1 = expr SLASH e2 = expr { EBinaryOp(e1, BDiv, e2) } [@name divide]
  | e1 = expr PERCENT e2 = expr { EBinaryOp(e1, BMod, e2) } [@name modulo]
  | e1 = expr EQ e2 = expr { EBinaryOp(e1, BEq, e2) } [@name equal]
  | e1 = expr NEQ e2 = expr { EBinaryOp(e1, BNe, e2) } [@name not_equal]
  | e1 = expr LT e2 = expr { EBinaryOp(e1, BLt, e2) } [@name less_than]
  | e1 = expr GT e2 = expr { EBinaryOp(e1, BGt, e2) } [@name greater_than]
  | e1 = expr LEQ e2 = expr { EBinaryOp(e1, BLe, e2) } [@name less_equal]
  | e1 = expr GEQ e2 = expr { EBinaryOp(e1, BGe, e2) } [@name greater_equal]
  | e1 = expr AND e2 = expr { EBinaryOp(e1, BAnd, e2) } [@name logical_and]
  | e1 = expr OR e2 = expr { EBinaryOp(e1, BOr, e2) } [@name logical_or]
  | NOT e = expr %prec UNOT { EUnaryOp(UNot, e) } [@name logical_not]
  | MINUS e = expr %prec UMINUS { EUnaryOp(UNeg, e) } [@name negation]
  | IF cond = expr then_branch = block_expr ELSE else_branch = block_expr
    { EIf(cond, then_branch, else_branch) } [@name if_else_expr]
  ;

/* 字面量表达式 */
literal:
  | b = INT_LIT { b } [@name integer_literal]
  ;