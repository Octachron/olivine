module type S = sig
  type t
  val view: t Ctypes.typ
end
module Make() =
struct
  type self
  type t = self Ctypes.structure Ctypes.ptr Ctypes.typ
  let t: t = Ctypes.ptr (Ctypes.structure "")
  let view = t
end
