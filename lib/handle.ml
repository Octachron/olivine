module type S = sig
  type t
  val t: t Ctypes.typ
  val view: t Ctypes.typ
  val null: t
end
module Make(): S =
struct
  type self
  type t = self Ctypes.structure Ctypes.ptr
  let t: t Ctypes.typ = Ctypes.ptr (Ctypes.structure "")
  let view = t
  let null = Ctypes.(coerce @@ ptr void) t Ctypes.null
end
