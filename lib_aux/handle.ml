module type S = sig
  type t
  val t: t Ctypes.typ
  val view: t Ctypes.typ
  val null: t
  val view_opt: t option Ctypes.typ
  val pp: Format.formatter -> t -> unit
  val array: t list -> t Ctypes.CArray.t
end
module Make(): S =
struct
  type self
  type t = self Ctypes.structure Ctypes.ptr
  let t: t Ctypes.typ = Ctypes.ptr (Ctypes.structure "")
  let view = t
  let null = Ctypes.(coerce @@ ptr void) t Ctypes.null
  let view_opt =
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
end
