type literal = CoqExtract.BrainFreeze.literal

let pp_literal fmt l = Format.fprintf fmt "%s" (Z.to_string l)

type binop = [%import: CoqExtract.BrainFreeze.binop] [@@deriving show { with_path = false }]

type unop = [%import: CoqExtract.BrainFreeze.unop] [@@deriving show { with_path = false }]

type expr = [%import: CoqExtract.BrainFreeze.expr] [@@deriving show { with_path = false }]

and statement = [%import: CoqExtract.BrainFreeze.statement] [@@deriving show { with_path = false }]

type program = [%import: CoqExtract.BrainFreeze.program] [@@deriving show { with_path = false }]
