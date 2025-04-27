Require Coq.extraction.Extraction.
Require Coq.extraction.ExtrOcamlBasic.
Require Coq.extraction.ExtrOcamlNativeString.
Require Coq.extraction.ExtrOcamlZBigInt.

Require Import Coq.Strings.String.
Require Import Coq.Lists.List.
Import ListNotations.

From BF Require Import BrainFreeze.

Extraction Language OCaml.
Extraction "CoqExtract" BrainFreeze.program_height.
