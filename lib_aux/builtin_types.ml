module U32 = Unsigned.UInt32
module U64 = Unsigned.UInt64
module S = Unsigned.Size_t

module type intlike = sig type t val zero: t end

let integer_opt (type a) (typ:a Ctypes.typ) (module I:intlike with type t = a) =
  let read x = if x = I.zero then None else Some x in
  let write = function None -> I.zero | Some x -> x in
  Ctypes.view ~read ~write typ

type uint_64_t = U64.t
let uint_64_t = Ctypes.uint64_t

(** {2 Uint32 special handling} *)
type uint_32_t = int (* ASSUME 64 bits *)
let uint_32_t = Ctypes.view ~read:(U32.to_int) ~write:(U32.of_int)
    Ctypes.uint32_t
module Int = struct type t = int let zero = 0 end
let uint_32_t_opt = integer_opt uint_32_t (module Int)
let int_32_t = Ctypes.int32_t

let uint_8_t = Ctypes.uint8_t


let bool_32 =
  let true' = U32.of_int Vk__const.true'
  and false' = U32.of_int Vk__const.false' in
  Ctypes.view
    ~read:( (=) true' )
    ~write:( fun x -> if x then true' else false' )
    Ctypes.uint32_t

let bool = bool_32

let size_t_opt = integer_opt Ctypes.size_t (module S)
let device_size_opt = integer_opt Ctypes.uint64_t (module U64)

module Size_t = struct let of_int = S.of_int let to_int = S.to_int end
module Uint_32_t = struct let of_int x = x let to_int x = x end
module Uint_64_t = struct let of_int =  U64.of_int let to_int = U64.to_int end

let unwrap = function
  | Some x -> x
  | None -> raise (Invalid_argument "unwrap None")


module type aliased = sig
  type t
  val ctype:t Ctypes.typ
  val of_int: int -> t
  val to_int: t -> int
end

module Alias(X:aliased): sig
  type t = private X.t
  val make: X.t -> t
  val ctype: t Ctypes.typ
  val of_int : int -> t
  val to_int: t -> int
end = struct
  type t = X.t
  let make x = x
  let ctype = X.ctype
  let of_int = X.of_int
  let to_int = X.to_int
end

let nullptr typ = Ctypes.(coerce (ptr void) (ptr typ) null)
