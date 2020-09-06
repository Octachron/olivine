open Info
let fp = Format.fprintf
let stdout = Format.std_formatter

module N = Common.StringMap

let read filename =
  let spec = open_in filename in
  let source = Xmlm.(make_input @@ `Channel spec) in
  Structured_spec.typecheck @@ Xml.normalize @@ snd @@ Xml.tree source

let () =
  (*   then *)
  let info = read Sys.argv.(1) in
  if Array.length Sys.argv > 2 then
    let query = Sys.argv.(2) in
    Structured_spec.pp_entity stdout @@ (query, N.find query info.entities)
  else
    Structured_spec.pp stdout info

    (*    let counts = Name_study.count_names info.entities in
    let pp_count ppf (name,n) = fp ppf "%s(%d)" name n in
    let binds = List.filter (fun x -> snd x > 4) (N.bindings counts) in
      fp stdout "@[<v>%a@]@." (Fmt.list pp_count) binds *)
    (* fp stdout "%a@." pp_nametree  (nametree info.entities) *)
    (*   *)
