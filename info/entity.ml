type t =
  | Const of Refined_types.Arith.t
  | Type of Refined_types.Ty.typexpr
  | Fn of Refined_types.Ty.fn
