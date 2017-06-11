module U32 = Unsigned.UInt32
module U64 = Unsigned.UInt64
module S = Unsigned.Size_t

module type intlike = sig type t val zero: t val ctype: t Ctypes.typ end

let integer_opt (type a) (module I:intlike with type t = a) =
  let read x = if x = I.zero then None else Some x in
  let write = function None -> I.zero | Some x -> x in
  Ctypes.view ~read ~write I.ctype

type uint_64_t = U64.t
let uint_64_t = Ctypes.uint64_t

(** {2 Uint32 special handling} *)
type uint_32_t = int (* ASSUME 64 bits *)
let uint_32_t = Ctypes.view ~read:(U32.to_int) ~write:(U32.of_int)
    Ctypes.uint32_t

module Int = struct
  type t = int
  let zero = 0
  let pp = Format.pp_print_int
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

let uint_8_t = Ctypes.uint8_t


let bool_32 =
  let true' = U32.of_int Vk__const.true'
  and false' = U32.of_int Vk__const.false' in
  Ctypes.view
    ~read:( (=) true' )
    ~write:( fun x -> if x then true' else false' )
    Ctypes.uint32_t

let bool = bool_32

(*let device_size_opt = integer_opt Ctypes.uint64_t (module U64)*)

module Size_t = struct
  let of_int = S.of_int
  let to_int = S.to_int
  let zero = of_int 0
  let to_string = S.to_string
  let pp ppf x = Format.fprintf ppf "%s" (S.to_string x)

  type t = S.t
  let ctype = Ctypes.size_t
end
let size_t_opt = integer_opt (module Size_t)

module Uint_32_t = struct
  let zero = 0
  let of_int x = x
  let to_int x = x
  let to_string = string_of_int
  let pp ppf x = Format.fprintf ppf "%d" x

  type t = int
  let ctype = uint_32_t
end
let uint_32_t_opt = integer_opt (module Uint_32_t)

module Bool_32 = struct
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
end
let int_32_t = Int_32_t.ctype

module Uint_64_t = struct
  let of_int =  U64.of_int
  let to_int = U64.to_int
  let zero = of_int 0
  let to_string = U64.to_string
  let pp ppf x = Format.fprintf ppf "%s" (U64.to_string x)

  type t = U64.t
  let ctype = Ctypes.uint64_t
end

let unwrap = function
  | Some x -> x
  | None -> raise (Invalid_argument "unwrap None")


module type aliased = sig
  type t
  val zero: t
  val ctype:t Ctypes.typ
  val of_int: int -> t
  val to_int: t -> int
  val to_string: t -> string
end

module type alias = sig
  type x
  type t = private x
  val make: x -> t
  val ctype: t Ctypes.typ
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
  let of_int = X.of_int
  let to_int = X.to_int
  let pp ppf x = Format.fprintf ppf "%s" (X.to_string x)
end

let nullptr typ = Ctypes.(coerce (ptr void) (ptr typ) null)

let may f = function
  | None -> None
  | Some x -> Some(f x)

let maybe f = match f with
  | Some f -> may f
  | None -> fun _ -> None

let pp_opt pp ppf = function
  | None -> Format.fprintf ppf "None"
  | Some x -> Format.fprintf ppf "Some(%a)" pp x

let pp_array pp ppf a =
  let get n = Ctypes.CArray.get a n in
  let n = Ctypes.CArray.length a in
  Format.fprintf ppf "@[<hov 2>⟦";
  if n > 0 then begin
    pp ppf (get 0);
    for i = 1 to (n-1) do
      Format.fprintf ppf "@ ;%a" pp (get i)
    done;
    Format.fprintf ppf "⟧@]"
  end

let pp_ptr pp ppf x =
  Format.fprintf ppf "*%a" pp (Ctypes.(!@) x)

let pp_string ppf =
  Format.fprintf ppf "%s"

let pp_addr ppf v =
  Format.fprintf ppf "%s" @@ Nativeint.to_string
  @@ Ctypes.raw_address_of_ptr v

let pp_abstract ppf _ = Format.fprintf ppf "⟨abstr⟩"

module Float = struct
  let pp = Format.pp_print_float
end

module Void = struct
  let pp = pp_abstract
end

let array_opt n t =
  let read = may (fun x -> Ctypes.CArray.from_ptr x n) in
  let write = may Ctypes.CArray.start in
  Ctypes.view read write (Ctypes.ptr_opt t)
