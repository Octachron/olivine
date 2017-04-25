module type S = sig
  type t
  val view: t Ctypes.typ
end
module Make() =
struct
  type t = int
  let id x = x
  let view = Ctypes.view id id Ctypes.int
end
