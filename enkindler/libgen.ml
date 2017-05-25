module Dict = Name_study.Dict
module R = Name_study.M
module S = Misc.StringSet

let read filename =
  let spec = open_in filename in
  let source = Xmlm.(make_input @@ `Channel spec) in
  Typed.typecheck @@ Xml.normalize @@ snd @@ Xml.tree source


let add_word name dict =
  let open Name_study in
  { dict with words = Dict.add name dict.words }


let add role name dict =
  let open Name_study in
  { dict with
    words = Dict.add name dict.words;
    roles = R.add (String.lowercase_ascii name) role dict.roles;
  }

let add_ext x exts =
  S.add (String.lowercase_ascii x) exts

let vendor_tag (dict,out) (x:Typed.short_tag)=
  ( add Extension x.name dict,
    add_ext x.name out)

let empty =
  let open Name_study in
  { words = Dict.empty; roles = R.empty;
    context = { mu with prefix = ["vk"]} }

let add_post x dict =
  Name_study.{ dict with roles = R.add x Postfix dict.roles }

let add_pre x dict =
  Name_study.{ dict with roles = R.add x Prefix dict.roles }


let make_dict spec =
  let dict, exts =
    List.fold_left vendor_tag (empty,S.empty) spec.Typed.tags in
  dict
  |> add Main "RandR"
  |> add Main "RR"
  |> add  Main "ID"
  |> add Main "D3D12"
  |> add Main "MacOS"
  |> add Extension "EXT"
  |> add Extension "KHX"
  |> add Main "IOS"
  |> add Main "1D"
  |> add Main "2D"
  |> add Main "3D"
  |> add Main "1d"
  |> add Main "2d"
  |> add Main "3d"
  |> add Main "UUID"
  |> add_word "Win32"
  |> add_post "flags"
  |> add_post "flag"
  |> add_post "bits"
  |> add_post "bit"
  |> add_pre "vk"
  , exts |> add_ext "KHX" |> add_ext "EXT"

let preambule =
  "open Ctypes\n\
   let libvulkan = Dl.dlopen ~filename:\"libvulkan.so\" ~flags:Dl.[RTLD_NOW]\n\
   let foreign name = Foreign.foreign ~from:libvulkan (\"vk\"^name) \n\
   module Printer = Format\n\
  "

let () =
  let info = read Sys.argv.(1) in
  let dict, _exts = make_dict info in
  let root = Sys.argv.(2) in
  let lib = Lib_builder.generate root preambule dict info in
  Printers.lib lib
