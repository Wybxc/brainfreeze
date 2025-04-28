
type nat =
| O
| S of nat



val add : nat -> nat -> nat

val max : nat -> nat -> nat

val fold_right : ('a2 -> 'a1 -> 'a1) -> 'a1 -> 'a2 list -> 'a1

module BrainFreeze :
 sig
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

  val expr_height : expr -> nat

  val statement_height : statement -> nat

  type program =
  | PProgram of string * expr

  val program_height : program -> nat
 end
