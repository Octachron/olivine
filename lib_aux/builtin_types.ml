module U32 = Unsigned.UInt32
module U64 = Unsigned.UInt64
module S = Unsigned.Size_t

module type intlike = sig type t val zero: t end

let integer_opt (type a) (typ:a Ctypes.typ) (module I:intlike with type t = a) =
  let read x = if x = I.zero then None else Some x in
  let write = function None -> I.zero | Some x -> x in
  Ctypes.view ~read ~write typ

type uint64_t = U64.t

(** {2 Uint32 special handling} *)
type uint32_t = int (* ASSUME 64 bits *)
let uint32_t = Ctypes.view ~read:(U32.to_int) ~write:(U32.of_int)
    Ctypes.uint32_t
module Int = struct type t = int let zero = 0 end
let uint32_t_opt = integer_opt uint32_t (module Int)

let bool_3_2 =
  let true' = U32.of_int Vk__const.true'
  and false' = U32.of_int Vk__const.false' in
  Ctypes.view
    ~read:( (=) true' )
    ~write:( fun x -> if x then true' else false' )
    Ctypes.uint32_t

let bool = bool_3_2

let size_t_opt = integer_opt Ctypes.size_t (module S)
let device_size_opt = integer_opt Ctypes.uint64_t (module U64)

module Size_t = struct let of_int = S.of_int end
module Uint32_t = struct let of_int x = x end
module Uint64_t = struct let of_int =  U64.of_int end


module type aliased = sig
  type t
  val ctype:t Ctypes.typ
  val of_int: int -> t
end

module Alias(X:aliased): sig
  type t = private X.t
  val make: X.t -> t
  val ctype: t Ctypes.typ
  val of_int : int -> t
end =
struct type t = X.t let make x = x let ctype = X.ctype let of_int = X.of_int  end

let nullptr typ = Ctypes.(coerce (ptr void) (ptr typ) null)
