module L = Info.Linguistic
module Dict = L.Dict
module R = L.M
module I = Aster.Item

let read filename =
  let spec = open_in filename in
  let source = Xmlm.(make_input @@ `Channel spec) in
  Info.Structured_spec.typecheck @@ Info.Xml.normalize @@ snd @@ Info.Xml.tree source




(*
let preambule =
  I.item
  [%str
    open Ctypes
    let libvulkan = Dl.dlopen ~filename:"libvulkan.so"
    ~flags:Dl.[RTLD_NOW]
    let foreign name = Foreign.foreign ~from:libvulkan name
    module Printer = Format
    module Option = struct
      type 'a t = 'a option = None | Some of 'a
    end
  ]
[%sig:
  open Ctypes
  module Printer = Format
]*)

let () =
  let info = read Sys.argv.(1) in
  let dict, _exts = Info.Vulkan_dialect.make info in
  let lib = Aster.Lib.generate (I.item [] []) dict info in
  if Array.length Sys.argv > 2 then (
    let root = Sys.argv.(2) in
    Printer.lib root lib
  ) else (
    Printer.dune_rule lib
  )
