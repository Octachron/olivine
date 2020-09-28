module L = Linguistic
module Dict = L.Dict
module R = L.M
module S = Common.StringSet


let add_word ?custom name dict =
  let open L in
  { dict with words = Dict.add name ?custom dict.words }


let add role ?custom name dict =
  let open L in
  { dict with
    words = Dict.add name ?custom dict.words;
    roles = R.add (String.lowercase_ascii name) role dict.roles;
  }

let add_ext x exts =
  S.add (String.lowercase_ascii x) exts


let vendor_tag (dict,out) (x:Structured_spec.short_tag)=
  ( add Extension x.name dict,
    add_ext x.name out)

let empty =
  let open L in
  { words = Dict.empty; roles = R.empty;
    context = { mu with prefix = ["vk"]} }

let add_post x dict =
  L.{ dict with roles = R.add x Postfix dict.roles }

let add_pre x dict =
  L.{ dict with roles = R.add x Prefix dict.roles }


let make spec =
  let dict, exts =

    List.fold_left vendor_tag (empty,S.empty) spec.Structured_spec.tags in
  dict
  |> add Main "RandR"
  |> add Main "RR"
  |> add Main "DirectFB"
  |> add Main "FB"
  |> add Main "D3D12"
  |> add Main "MacOS"
  |> add Main "LOD"
  |> add Main "ID"
  |> add Extension "EXT"
  |> add Extension "KHX"
  |> add Main "IOS"
  |> add Main "1D"
  |> add Main "2D"
  |> add Main "3D"
  |> add Main "1d"
  |> add Main "2d"
  |> add Main "3d"
  |> add Main "16Bit"
  |> add Main "8Bit"
  |> add Main "UUID"
  |> add_word "AABB"
  |> add_word "CAMetalLayer"
  |> add_word "SM"
  |> add_word ~custom:["vulkan";"1";"1"] "Vulkan11"
  |> add_word ~custom:["vulkan";"1";"2"] "Vulkan12"
  |> add Main "ASTC"
  |> add Main "HDR"
  |> add_word "PCI"
  |> add_word "Win32"
  |> add_post "flags"
  |> add_post "flag"
  |> add_post "bits"
  |> add_post "bit"
  |> add_pre "vk"
  , exts |> add_ext "KHX" |> add_ext "EXT"

