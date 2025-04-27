Require Import Coq.ZArith.ZArith.
Require Import Coq.Lists.List.
Require Import Coq.Strings.String.

From BF Require Import Int.

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
| ELiteral : literal -> expr
| EVariable : string -> expr
| EBinaryOp : expr -> binop -> expr -> expr
| EUnaryOp : unop -> expr -> expr
| EIf : expr -> expr -> option expr -> expr
| EBlock : list statement -> expr -> expr
with statement :=
| SLet : string -> option expr -> statement
| SAssign : string -> expr -> statement
| SExpr : expr -> statement
| SIf : expr -> statement -> option statement -> statement
| SWhile : expr -> statement -> statement
| SReturn : statement
| SBlock : list statement -> statement
.

Fixpoint expr_height (e : expr) : nat :=
  match e with
  | ELiteral _ => 1
  | EVariable _ => 1
  | EBinaryOp e1 _ e2 =>
      let h1 := expr_height e1 in
      let h2 := expr_height e2 in
      max h1 h2 + 1
  | EUnaryOp _ e =>
      expr_height e + 1
  | EIf cond then_branch else_branch =>
      let h_cond := expr_height cond in
      let h_then := expr_height then_branch in
      match else_branch with
      | None => max h_cond h_then + 1
      | Some else_branch =>
        let h_else := expr_height else_branch in
        max (max h_cond h_then) h_else + 1
      end
  | EBlock stmts ret =>
      let h_stmts := fold_right (fun s acc => max (statement_height s) acc) 0 stmts in
      max h_stmts (expr_height ret) + 1
  end
with statement_height (s : statement) : nat :=
  match s with
  | SLet _ e =>
      match e with
      | None => 1
      | Some e => expr_height e + 1
      end
  | SAssign _ e => expr_height e + 1
  | SExpr e => expr_height e + 1
  | SIf cond then_branch else_branch =>
      let h_cond := expr_height cond in
      let h_then := statement_height then_branch in
      match else_branch with
      | None => max h_cond h_then + 1
      | Some else_branch =>
        let h_else := statement_height else_branch in
        max (max h_cond h_then) h_else + 1
      end
  | SWhile cond body =>
      let h_cond := expr_height cond in
      let h_body := statement_height body in
      max h_cond h_body + 1
  | SReturn => 1
  | SBlock stmts =>
      fold_right (fun s acc => max (statement_height s) acc) 0 stmts + 1
  end.

Inductive program :=
| PProgram : list statement -> program
.

Definition program_height (p : program) : nat :=
  match p with
  | PProgram stmts =>
      fold_right (fun s acc => max (statement_height s) acc) 0 stmts
  end.

End BrainFreeze.
