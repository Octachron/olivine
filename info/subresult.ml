

module Atoms = struct
  type t = string list
  let compare (x:t) (y:t) = compare x y
end
module S = Set.Make(Atoms)
module L = Linguistic

let atoms_of_name name =
    let open L in
    remove_prefix [ "error"]
    @@ remove_prefix [ "vk"]
    @@ to_path name

  let atoms names =
    List.fold_left (fun set x -> S.add (atoms_of_name x) set  )
      S.empty names

  let flat = List.map (String.concat "'")

  let composite_path ok errors  =
     flat @@ S.elements @@ S.union (atoms ok) (atoms errors)

  let composite_name ok errors =
    String.concat "_" @@ composite_path ok errors

let composite_nominal ok errors =
  L.simple @@ composite_path ok errors

  let side_name constrs =
    L.simple @@ flat @@ S.elements @@ atoms @@ constrs
