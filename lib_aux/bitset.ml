

type singleton = private Singleton
type plural = private Plural

module type S = sig

  type +'a set
  type index = singleton set
  type t = plural set
  exception Out_of_bound of int

  val empty: 'a set
  val make_index: int -> 'a set
  val mem: index -> 'a set -> bool

  val union: 'a set -> 'a set -> t
  val (+): 'a set -> 'a set -> t

  val diff: 'a set -> 'a set -> t
  val (-): 'a set -> 'a set -> t

  val not: 'a set -> t
  val (~-): 'a set -> t

  val intersection: 'a set -> 'a set -> t
  val ( * ) : 'a set -> 'a set -> t

  val of_int: int -> 'a set
  val view: t Ctypes.typ
  val view_opt: t option Ctypes.typ
  val index_view: index Ctypes.typ
  val index_view_opt: index option Ctypes.typ

  val pp: Format.formatter -> t -> unit
end

module Make(): S = struct
  type 'a set = int
  type index = singleton set
  type t = plural set
  exception Out_of_bound of int

  let empty = 0

  let make_index n =
    if n > 63 then
      raise @@ Out_of_bound n
    else
      1 lsl n

  let mem n x =
    x land n <> 0

  let union = (lor)
  let (+) = union

  let not = lnot let (~-) = not
  let diff x y = x lor not y
  let (-) = diff

  let intersection = (land)
  let ( * ) = intersection

  let of_int n = n
  let id x = x
  let view = Ctypes.view id id Ctypes.int

  let view_opt =
    let read x = if x = 0 then None else Some x in
    let write = function None -> 0 | Some x -> x in
    Ctypes.view read write Ctypes.int

  let index_view = Ctypes.view id id Ctypes.int

  let index_view_opt =
    let read x = if x = 0 then None else Some x in
    let write = function None -> 0 | Some x -> x in
    Ctypes.view read write Ctypes.int

  let pp ppf _ = Format.pp_print_string ppf "ø"
end
