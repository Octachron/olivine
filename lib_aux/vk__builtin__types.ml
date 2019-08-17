module U32 = Unsigned.UInt32
module U64 = Unsigned.UInt64
module U16 = Unsigned.UInt16
module S = Unsigned.Size_t

module type intlike = sig type t val zero: t val ctype: t Ctypes.typ end

let integer_opt (type a) (module I:intlike with type t = a) =
  let read x = if x = I.zero then None else Some x in
  let write = function None -> I.zero | Some x -> x in
  Ctypes.view ~read ~write I.ctype

let integer_opt' zero ctype =
  let read x = if x = zero then None else Some x in
  let write = function None -> zero | Some x -> x in
  Ctypes.view ~read ~write ctype


type uint_64_t = U64.t
let uint_64_t = Ctypes.uint64_t

(** {2 Uint32 special handling} *)
type uint_32_t = int (* ASSUME 64 bits *)
let uint_32_t = Ctypes.view ~read:(U32.to_int) ~write:(U32.of_int)
    Ctypes.uint32_t

type uint_16_t = int (* ASSUME 64 bits *)
let uint_16_t = Ctypes.view ~read:(U16.to_int) ~write:(U16.of_int)
    Ctypes.uint16_t


type void = unit
let void = Ctypes.void

type int_32_t = int
type bool_32 = bool

module Int = struct
  type t = int
  let zero = 0
  let pp = Format.pp_print_int
  let ctype = Ctypes.int
end

module U8 = Unsigned.UInt8
module Uint_8_t = struct
  open U8
  type t = U8.t
  let ctype = Ctypes.uint8_t
  let zero = of_int 0
  let of_int = of_int
  let to_int = to_int
  let to_string = to_string
  let pp ppf x = Format.pp_print_string ppf (to_string x)
end

type uint_8_t = U8.t
let uint_8_t = Ctypes.uint8_t


let bool_32 =
  let true' = U32.of_int Vk__Const.true'
  and false' = U32.of_int Vk__Const.false' in
  Ctypes.view
    ~read:( (=) true' )
    ~write:( fun x -> if x then true' else false' )
    Ctypes.uint32_t

let bool = bool_32

(*let device_size_opt = integer_opt Ctypes.uint64_t (module U64)*)

module Size_t_0 = struct
  let of_int = S.of_int
  let to_int = S.to_int
  let zero = of_int 0
  let to_string = S.to_string
  let pp ppf x = Format.fprintf ppf "%s" (S.to_string x)

  type t = S.t
  let ctype = Ctypes.size_t
end

module Size_t = struct
  include Size_t_0
  let ctype_opt = integer_opt (module Size_t_0)
end

type size_t = Size_t.t
let size_t = Size_t.ctype
let size_t_opt = integer_opt (module Size_t)

module Uint_32_t_0 = struct
  let zero = 0
  let of_int x = x
  let to_int x = x
  let to_string = string_of_int
  let pp ppf x = Format.fprintf ppf "%d" x

  type t = int
  let ctype = uint_32_t
end
module Uint_32_t = struct include Uint_32_t_0
  let ctype_opt = integer_opt (module Uint_32_t_0)
end

module Uint_16_t_0 = struct
  let zero = 0
  let of_int x = x
  let to_int x = x
  let to_string = string_of_int
  let pp ppf x = Format.fprintf ppf "%d" x

  type t = int
  let ctype = uint_16_t
end
module Uint_16_t = struct include Uint_16_t_0
  let ctype_opt = integer_opt (module Uint_16_t_0)
end

module Bool_32 = struct
  type t = bool
  let t = bool
  let ctype = bool_32
  let pp = Format.pp_print_bool
end

module Int_32_t = struct
  let zero = 0
  let of_int x = x
  let to_int x = x
  let to_string = string_of_int
  let pp ppf x = Format.fprintf ppf "%d" x

  type t = int
  let read = Int32.to_int
  let write = Int32.of_int
  let ctype = Ctypes.view ~read ~write Ctypes.int32_t
  let ctype_opt = integer_opt' zero ctype
end
let int_32_t = Int_32_t.ctype


module Int_64_t = struct
  let zero = 0
  let of_int x = x
  let to_int x = x
  let to_string = string_of_int
  let pp ppf x = Format.fprintf ppf "%d" x

  type t = int
  let read = Int64.to_int
  let write = Int64.of_int
  let ctype = Ctypes.view ~read ~write Ctypes.int64_t
  let ctype_opt = integer_opt' zero ctype
end
let int_64_t = Int_64_t.ctype


module Uint_64_t = struct
  let of_int =  U64.of_int
  let to_int = U64.to_int
  let zero = of_int 0
  let to_string = U64.to_string
  let pp ppf x = Format.fprintf ppf "%s" (U64.to_string x)

  type t = U64.t
  let ctype = Ctypes.uint64_t
  let ctype_opt = integer_opt' zero ctype
end



module type aliased = sig
  type t
  val zero: t
  val ctype:t Ctypes.typ
  val ctype_opt: t option Ctypes.typ
  val of_int: int -> t
  val to_int: t -> int
  val to_string: t -> string
end

module type alias = sig
  type x
  type t = private x
  val make: x -> t
  val ctype: t Ctypes.typ
  val ctype_opt: t option Ctypes.typ
  val of_int : int -> t
  val zero: t
  val to_int: t -> int
  val pp: Format.formatter -> t -> unit
end

module Alias(X:aliased): alias with type x := X.t = struct
  type t = X.t
  let make x = x
  let zero = X.zero
  let ctype = X.ctype
  let ctype_opt = X.ctype_opt
  let of_int = X.of_int
  let to_int = X.to_int
  let pp ppf x = Format.fprintf ppf "%s" (X.to_string x)
end

module Float = struct
  type t = float
  let pp = Format.pp_print_float
  let ctype = Ctypes.float
end


module Double = struct
  type t = float
  let pp = Format.pp_print_float
  let ctype = Ctypes.double
end


module Void = struct
  type t = void
  let ctype = Ctypes.void
  let pp = Vk__helpers.Pp.abstract
end

module Cametallayer = struct
  type m
  type t = m Ctypes.structure
  let ctype : t Ctypes.typ  = Ctypes.structure "CAmetallayer"
  let pp = Vk__helpers.Pp.abstract
end
type cametallayer = Cametallayer.t Ctypes.structure
