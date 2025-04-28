
type nat =
| O
| S of nat



(** val add : nat -> nat -> nat **)

let rec add n m =
  match n with
  | O -> m
  | S p -> S (add p m)

(** val max : nat -> nat -> nat **)

let rec max n m =
  match n with
  | O -> m
  | S n' -> (match m with
             | O -> n
             | S m' -> S (max n' m'))

(** val fold_right : ('a2 -> 'a1 -> 'a1) -> 'a1 -> 'a2 list -> 'a1 **)

let rec fold_right f a0 = function
| [] -> a0
| b :: t -> f b (fold_right f a0 t)

module BrainFreeze =
 struct
  type literal =
    Big_int_Z.big_int
    (* singleton inductive, whose constructor was LByte *)

  type binop =
  | BAdd
  | BSub
  | BMul
  | BDiv
  | BMod
  | BAnd
  | BOr
  | BEq
  | BLt
  | BGt
  | BLe
  | BGe
  | BNe

  type unop =
  | UNot
  | UNeg

  type expr =
  | EUnit
  | ELiteral of literal
  | EVariable of string
  | EAssign of string * expr
  | EBinaryOp of expr * binop * expr
  | EUnaryOp of unop * expr
  | EReturn of expr
  | EIf of expr * expr * expr option
  | EWhile of expr * expr
  | EBlock of statement list
  and statement =
  | SLet of string * expr option
  | SExpr of expr
  | SSemi of expr

  (** val expr_height : expr -> nat **)

  let rec expr_height = function
  | EAssign (_, e0) -> add (expr_height e0) (S O)
  | EBinaryOp (e1, _, e2) ->
    let h1 = expr_height e1 in
    let h2 = expr_height e2 in add (max h1 h2) (S O)
  | EUnaryOp (_, e0) -> add (expr_height e0) (S O)
  | EReturn e0 -> add (expr_height e0) (S O)
  | EIf (cond, then_branch, else_branch) ->
    let h_cond = expr_height cond in
    let h_then = expr_height then_branch in
    let h_else = match else_branch with
                 | Some e0 -> expr_height e0
                 | None -> O
    in
    add (max (max h_cond h_then) h_else) (S O)
  | EWhile (cond, body) ->
    let h_cond = expr_height cond in
    let h_body = expr_height body in add (max h_cond h_body) (S O)
  | EBlock stmts ->
    let h_stmts =
      fold_right (fun s acc -> max (statement_height s) acc) O stmts
    in
    add h_stmts (S O)
  | _ -> S O

  (** val statement_height : statement -> nat **)

  and statement_height = function
  | SLet (_, e) ->
    (match e with
     | Some e0 -> add (expr_height e0) (S O)
     | None -> S O)
  | SExpr e -> add (expr_height e) (S O)
  | SSemi e -> add (expr_height e) (S O)

  type program =
  | PProgram of string * expr

  (** val program_height : program -> nat **)

  let program_height = function
  | PProgram (_, e) -> expr_height e
 end
