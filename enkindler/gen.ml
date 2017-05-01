module Dict = Name_study.Dict

let read filename =
  let spec = open_in filename in
  let source = Xmlm.(make_input @@ `Channel spec) in
  Typed.typecheck @@ Xml.normalize @@ snd @@ Xml.tree source

let vendor_tag dict (x:Typed.short_tag)=
  Dict.add x.name dict

let make_dict spec =
  List.fold_left vendor_tag Dict.empty spec.Typed.tags
  |> Dict.add "RandR"
  |> Dict.add "RR"
  |> Dict.add "ID"

let () =
  let info = read Sys.argv.(1) in
  let dict = make_dict info in
  let output = Format.formatter_of_out_channel
    @@ open_out Sys.argv.(2) in
  Generator.make_all dict output info.entities
