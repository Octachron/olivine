type t =
  | Const of Refined_types.Arith.t
  | Type of Refined_types.Ty.typedef
  | Fn of Refined_types.Ty.fn
