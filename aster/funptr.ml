
module Aliases= struct
  module L = Info.Linguistic
  module Ty = Lib.Ty
  module C = Common
end
open Aliases
open Item
open Utils

let mkty ctx args ret =
  let ret  = Type.converter ctx ~degraded:true ret in
  let fn = C.listr (fun l r -> [%expr[%e l] @-> [%e r] ])
    (Type.converter ctx ~degraded:true)
    args
    [%expr returning [%e ret]] in
  [%expr let open Ctypes in [%e fn] ]

let expand = function
  | [] -> [Ty.Name (L.simple ["void"])]
  | l -> l

let view = L.(~:"ctype")
let make ctx (_tyname, (fn:Ty.fn)) =
  let ty = pty view and tyo = pty L.(view//"opt")in
  match List.map snd @@ Ty.flatten_fn_fields fn.args with
  | [] ->
    let typ =  [%type: unit Ctypes.ptr] in
    decltype ~manifest:typ "t"
    ^:: item
            [[%stri let [%p ty] = Ctypes.(ptr void)]]
            [val' view [%type: [%t typ] Ctypes.typ] ]
  | args ->
    let t = Type.fn2 ~decay_array:All ~mono:true ctx fn in
    decltype ~manifest:t "t"
    ^:: item
      [[%stri let [%p ty], [%p tyo] =
                let ty = [%e mkty ctx args fn.return] in
                Foreign.funptr ty, Foreign.funptr_opt ty
      ]]
      [ val' view [%type: t Ctypes.typ];
        val' L.(view//"opt") [%type: t option Ctypes.typ];
      ]
