module U32 = Unsigned.UInt32
module U64 = Unsigned.UInt64
module S = Unsigned.Size_t

module type intlike = sig type t val zero: t end

let integer_opt (type a) (typ:a Ctypes.typ) (module I:intlike with type t = a) =
  let read x = if x = I.zero then None else Some x in
  let write = function None -> I.zero | Some x -> x in
  Ctypes.view ~read ~write typ

let uint32_t_opt = integer_opt Ctypes.uint32_t (module U32)
let size_t_opt = integer_opt Ctypes.size_t (module S)
let device_size_opt = integer_opt Ctypes.uint64_t (module U64)

module type aliased = sig type t val ctype:t Ctypes.typ end

module Alias(X:aliased): sig
  type t = private X.t
  val ctype: t Ctypes.typ
end =
struct type t = X.t let ctype = X.ctype end
