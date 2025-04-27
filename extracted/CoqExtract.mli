
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
  | ELiteral of literal
  | EVariable of string
  | EBinaryOp of expr * binop * expr
  | EUnaryOp of unop * expr
  | EIf of expr * expr * expr option
  | EBlock of statement list * expr
  and statement =
  | SLet of string * expr option
  | SAssign of string * expr
  | SExpr of expr
  | SIf of expr * statement * statement option
  | SWhile of expr * statement
  | SReturn
  | SBlock of statement list

  val expr_height : expr -> nat

  val statement_height : statement -> nat

  type program =
    statement list
    (* singleton inductive, whose constructor was PProgram *)

  val program_height : program -> nat
 end
