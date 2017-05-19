
module type S = sig
  type t
  type index
  exception Out_of_bound of int

  val empty: t
  val make_index: int -> index
  val mem: index -> t -> bool

  val union: t -> t -> t
  val (+): t -> t -> t

  val diff: t -> t -> t
  val (-): t -> t -> t

  val not: t -> t
  val (~-): t -> t

  val intersection: t -> t -> t
  val ( * ) : t -> t -> t

  val singleton: index -> t
  val of_list: index list -> t

  val of_int: int -> t
  val view: t Ctypes.typ
  val index_view: index Ctypes.typ
end

module Make(): S = struct
  type t = int
  type index = int
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

  let singleton k = k
  let of_list =
    List.fold_left ( fun acc x -> acc + singleton x) empty

  let of_int n = n
  let id x = x
  let view = Ctypes.view id id Ctypes.int
  let index_view = Ctypes.view id id Ctypes.int
end
