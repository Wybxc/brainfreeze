Require Extraction.
Require ExtrOcamlBasic.
Require ExtrOcamlNativeString.

Require Import String.
Require Import List.
Import ListNotations.

(* 定义要提取的函数 *)
Definition greet (name : string) : string :=
  "Hello, " ++ name ++ "!".

(* 提取设置 *)
Extraction Language OCaml.
Extraction "CoqExtract" greet.
