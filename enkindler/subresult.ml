
module S = Misc.StringSet
module L = Name_study

let atoms_of_name name =
    let open Name_study in
    S.of_list @@ List.map fst
    @@ remove_prefix [ word "error"]
    @@ remove_prefix [ word "vk"]
    @@ to_path name

  let atoms names =
    List.fold_left (fun set x -> S.union set @@ atoms_of_name x )
      S.empty names

  let composite_path ok errors  =
    S.elements @@ S.union (atoms ok) (atoms errors)

  let composite_name ok errors =
    String.concat "_" @@ composite_path ok errors

let composite_nominal ok errors =
  L.simple @@ List.map L.word @@ composite_path ok errors

  let side_name constrs =
    L.simple @@ List.map L.word @@ S.elements @@ atoms @@ constrs

