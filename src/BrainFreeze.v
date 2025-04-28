Require Import Coq.ZArith.ZArith.
Require Import Coq.Lists.List.
Require Import Coq.Strings.String.

Module BrainFreeze.

Local Open Scope string_scope.

Inductive literal :=
| LByte : Z -> literal
.

Inductive binop :=
| BAdd : binop
| BSub : binop
| BMul : binop
| BDiv : binop
| BMod : binop
| BAnd : binop
| BOr : binop
| BEq : binop
| BLt : binop
| BGt : binop
| BLe : binop
| BGe : binop
| BNe : binop
.

Inductive unop :=
| UNot : unop
| UNeg : unop
.

Inductive expr :=
| EUnit     : expr
| ELiteral  : literal -> expr
| EVariable : string -> expr
| EAssign   : string -> expr -> expr
| EBinaryOp : expr -> binop -> expr -> expr
| EUnaryOp  : unop -> expr -> expr
| EReturn   : expr -> expr
| EIf       : expr -> expr -> option expr -> expr
| EWhile    : expr -> expr -> expr
| EBlock    : list statement -> expr
with statement :=
| SLet  : string -> option expr -> statement
| SExpr : expr -> statement
| SSemi : expr -> statement
.

Fixpoint expr_height (e : expr) : nat :=
  match e with
  | EUnit => 1
  | ELiteral _ => 1
  | EVariable _ => 1
  | EAssign _ e => expr_height e + 1
  | EBinaryOp e1 _ e2 =>
      let h1 := expr_height e1 in
      let h2 := expr_height e2 in
      max h1 h2 + 1
  | EUnaryOp _ e =>
      expr_height e + 1
  | EReturn e => expr_height e + 1
  | EIf cond then_branch else_branch =>
      let h_cond := expr_height cond in
      let h_then := expr_height then_branch in
      let h_else :=
        match else_branch with
        | None => 0
        | Some e => expr_height e
        end in
      max (max h_cond h_then) h_else + 1
  | EWhile cond body =>
      let h_cond := expr_height cond in
      let h_body := expr_height body in
      max h_cond h_body + 1
  | EBlock stmts =>
      let h_stmts := fold_right (fun s acc => max (statement_height s) acc) 0 stmts in
      h_stmts + 1
  end
with statement_height (s : statement) : nat :=
  match s with
  | SLet _ e =>
      match e with
      | None => 1
      | Some e => expr_height e + 1
      end
  | SExpr e => expr_height e + 1
  | SSemi e => expr_height e + 1
  end.

Inductive program :=
| PProgram : string -> expr -> program
.

Definition program_height (p : program) : nat :=
  match p with
  | PProgram _ e => expr_height e
  end.

End BrainFreeze.
