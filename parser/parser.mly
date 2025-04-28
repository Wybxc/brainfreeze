%{
  open Ast
%}

/* 标记声明 */
%token <Z.t> INT_LIT
%token <string> IDENT
// %token <string> STRING_LIT
// %token BYTE_T
%token FN
%token LET
%token IF ELSE
%token WHILE
// %token FOR IN
%token RETURN
// %token LBRACKET RBRACKET
%token LBRACE RBRACE
%token LPAREN RPAREN
%token SEMICOLON
// %token COLON COMMA
%token PLUS MINUS STAR SLASH PERCENT
%token EQ NEQ LT GT LEQ GEQ
%token ASSIGN
%token AND OR NOT
%token EOF

/* 添加操作符优先级和结合性规则 */
// %nonassoc THEN
// %nonassoc ELSE

%nonassoc RETURN
%nonassoc ASSIGN
%left OR
%left AND
%nonassoc EQ NEQ LT GT LEQ GEQ
%left PLUS MINUS
%left STAR SLASH PERCENT
%right NOT
%right U_MINUS
%nonassoc STANDALONE

/* 入口点 */
%start <program> program

%on_error_reduce statement_item expr standalone_expr

%%

program:
  | fns = func_def EOF { fns } [@name program]
  ;

func_def:
  | FN name=IDENT LPAREN RPAREN body = block
    { PProgram(name, body) } [@name func_def]
  ;

block:
  | LBRACE RBRACE
    { EBlock([]) } [@name empty_block]
  | LBRACE stmts = statement_list RBRACE
    { EBlock(stmts) } [@name block]
  ;

statement_list:
  | e = statement_item { [e] } [@name item_stmt_list]
  | e = expr
    { [SExpr(e)] } [@name expr_stmt_list]
  | e = expr SEMICOLON
    { [SSemi(e)] } [@name semi_stmt_list]
  | e = standalone_expr s = statement_list
    { SExpr(e) :: s } [@name expr_cons_stmt_list]
  | e = expr SEMICOLON s = statement_list
    { SSemi(e) :: s } [@name semi_cons_stmt_list]
  | i = statement_item SEMICOLON s = statement_list
    { i :: s } [@name item_cons_stmt_list]
  ;

statement_item:
  | LET name = IDENT
    init = option(preceded(ASSIGN, expr))
    SEMICOLON
    { SLet(name, init) } [@name let_stmt]
  ;

standalone_expr:
  | e = block { e } [@name block_expr]
  | IF cond = expr then_branch = block
    { EIf(cond, then_branch, None) } [@name if_expr]
  | IF cond = expr then_branch = block ELSE else_branch = block
    { EIf(cond, then_branch, Some else_branch) } [@name if_else_expr]
  | WHILE cond = expr body = block
    { EWhile(cond, body) } [@name while_expr]

expr:
  | LPAREN RPAREN { EUnit } [@name unit]
  | id = IDENT { EVariable(id) } [@name variable]
  | lit = literal { ELiteral(lit) } [@name literal]
  | LPAREN e = expr RPAREN { e } [@name paren]
  | e1 = IDENT ASSIGN e2 = expr { EAssign(e1, e2) } [@name assign]
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
  | NOT e = expr { EUnaryOp(UNot, e) } [@name logical_not]
  | MINUS e = expr %prec U_MINUS { EUnaryOp(UNeg, e) } [@name negation]
  | RETURN e = expr { EReturn(e) } [@name return_expr]
  | e = standalone_expr { e } %prec STANDALONE [@name standalone_expr]
  ;

/* 字面量表达式 */
literal:
  | b = INT_LIT { b } [@name integer_literal]
  ;
