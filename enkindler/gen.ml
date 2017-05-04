module Dict = Name_study.Dict
module R = Name_study.M

let read filename =
  let spec = open_in filename in
  let source = Xmlm.(make_input @@ `Channel spec) in
  Typed.typecheck @@ Xml.normalize @@ snd @@ Xml.tree source


let add name dict =
  let open Name_study in
  { words = Dict.add name dict.words;
    roles = R.add name Postfix dict.roles }

let vendor_tag dict (x:Typed.short_tag)=
  add x.name dict

let empty =
  let open Name_study in
  { words = Dict.empty; roles = R.empty }

let add_post x dict =
  Name_study.{ dict with roles = R.add x Postfix dict.roles }

let make_dict spec =
  List.fold_left vendor_tag empty spec.Typed.tags
  |> add "RandR"
  |> add "RR"
  |> add "ID"
  |> add "EXT"
  |> add_post "flags"
  |> add_post "flag"
  |> add_post "bits"

let () =
  let info = read Sys.argv.(1) in
  let dict = make_dict info in
  let output = Format.formatter_of_out_channel
    @@ open_out Sys.argv.(2) in
  Generator.make_all dict output info.entities
