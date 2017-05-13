module Dict = Name_study.Dict
module R = Name_study.M
module S = Generator.S

let read filename =
  let spec = open_in filename in
  let source = Xmlm.(make_input @@ `Channel spec) in
  Typed.typecheck @@ Xml.normalize @@ snd @@ Xml.tree source


let add_word name dict =
  let open Name_study in
  { dict with words = Dict.add name dict.words }


let add name dict =
  let open Name_study in
  { dict with
    words = Dict.add name dict.words;
    roles = R.add (String.lowercase_ascii name) Postfix dict.roles;
  }

let add_ext x exts =
  S.add (String.lowercase_ascii x) exts

let vendor_tag (dict,out) (x:Typed.short_tag)=
  ( add x.name dict,
    add_ext x.name out)

let empty =
  let open Name_study in
  { words = Dict.empty; roles = R.empty;
    context = { mu with prefix = [word "vk"]} }

let add_post x dict =
  Name_study.{ dict with roles = R.add x Postfix dict.roles }

let add_pre x dict =
  Name_study.{ dict with roles = R.add x Prefix dict.roles }


let make_dict spec =
  let dict, exts =
    List.fold_left vendor_tag (empty,S.empty) spec.Typed.tags in
  dict
  |> add "RandR"
  |> add "RR"
  |> add "ID"
  |> add "EXT"
  |> add "KHX"
  |> add "IOS"
  |> add "1D"
  |> add "2D"
  |> add "3D"
  |> add "UUID"
  |> add_word "Win32"
  |> add_post "flags"
  |> add_post "flag"
  |> add_post "bits"
  |> add_post "bit"
  |> add_pre "vk"
  , exts |> add_ext "KHX" |> add_ext "EXT"

let () =
  let info = read Sys.argv.(1) in
  let dict, exts = make_dict info in
  let root = Sys.argv.(2) in
  Generator.make_all exts dict root info.entities
