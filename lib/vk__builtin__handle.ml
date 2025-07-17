
module type core = sig
  type t
  val t: t Ctypes.typ
  val ctype: t Ctypes.typ
  val null: t
  val ctype_opt: t option Ctypes.typ
  val pp: Format.formatter -> t -> unit
  val array: t list -> t Ctypes.CArray.t
  val array_opt: t list -> t option Ctypes.CArray.t
end

module type S = sig
  include core
  val to_ptr: t -> nativeint
  val unsafe_from_ptr: nativeint -> t
end

module type S_non_dispatchable = sig
  include core
  val to_int64: t -> int64
  val unsafe_from_int64: int64 -> t
end

module Make(): S  =
struct
  type self
  type t = self Ctypes.structure Ctypes.ptr
  let t: t Ctypes.typ = Ctypes.ptr (Ctypes.structure "")
  let ctype = t
  let null = Ctypes.(coerce @@ ptr void) t Ctypes.null
  let ctype_opt =
    let read v = if v = null then None else Some v in
    let write = function
      | None -> null
      | Some x -> x in
    Ctypes.view ~read ~write t
  let pp ppf (x:t) =
    Format.fprintf ppf "%s" @@ Nativeint.to_string
    @@ Ctypes.( raw_address_of_ptr @@ coerce t (ptr void) x)

  let array l =
    let a = Ctypes.CArray.of_list t l in
    Gc.finalise ( fun _ -> let _kept_alive = ref l in () ) a;
    a

  let array_opt l =
    let a = Ctypes.CArray.of_list ctype_opt (List.map (fun x -> Some x) l) in
    Gc.finalise ( fun _ -> let _kept_alive = ref l in () ) a;
    a


  let to_ptr x = Ctypes.raw_address_of_ptr @@ Ctypes.coerce t Ctypes.(ptr void) x
  let unsafe_from_ptr x =
    Ctypes.coerce Ctypes.(ptr void) t @@ Ctypes.ptr_of_raw_address x
end

module Make_non_dispatchable(): S_non_dispatchable =
struct
  type self
  type t = int64
  let t: t Ctypes.typ = Ctypes.int64_t
  let ctype = t
  let null = 0L
  let ctype_opt =
    let read v = if v = null then None else Some v in
    let write = function
      | None -> null
      | Some x -> x in
    Ctypes.view ~read ~write t
  let pp ppf (x:t) =
    Format.fprintf ppf "%s" @@ Nativeint.to_string
    @@ Ctypes.( raw_address_of_ptr @@ coerce t (ptr void) x)

  let array l =
    let a = Ctypes.CArray.of_list t l in
    Gc.finalise ( fun _ -> let _kept_alive = ref l in () ) a;
    a

  let array_opt l =
    let a = Ctypes.CArray.of_list ctype_opt (List.map (fun x -> Some x) l) in
    Gc.finalise ( fun _ -> let _kept_alive = ref l in () ) a;
    a

  let to_int64 x = x
  let unsafe_from_int64 x = x
end
