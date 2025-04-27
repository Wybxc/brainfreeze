Require Import Coq.ZArith.ZArith.
Require Import Coq.Lists.List.
Require Import Coq.Strings.String.
Require Import Recdef.

Local Open Scope Z_scope.
Local Open Scope list_scope.
Local Open Scope string_scope.

Axiom string_of_Z : Z -> string.

Extract Constant string_of_Z => "Big_int_Z.string_of_big_int".
