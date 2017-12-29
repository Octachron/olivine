
module Aliases= struct
  module L = Name_study
  module Ty = Lib_builder.Ty
  module C = Ast__common
end
open Aliases
open Ast__item
open Ast__utils

let mkty ctx args ret =
  let ret  = Ast__type.converter ctx true ret in
  C.listr (fun l r -> [%expr[%e l] @-> [%e r] ])
    (Ast__type.converter ctx ~degraded:true)
    args
    [%expr returning [%e ret]]

let expand = function
  | [] -> [Ty.Name (L.simple ["void"])]
  | l -> l

let make ctx (tyname, (fn:Ty.fn)) =
  let ty = pty tyname and tyo = pty L.(tyname//"opt")in
  match List.map snd @@ Ty.flatten_fn_fields fn.args with
  | [] ->
    let typ =  [%type: unit Ctypes.ptr] in
    decltype ~manifest:typ (typestr fn.name)
    ^:: item
            [[%stri let [%p ty] = ptr void]]
            [val' tyname [%type: [%t typ] Ctypes.typ] ]
  | args ->
    let t = Ast__type.fn2 ~decay_array:All ~mono:true ctx fn in

    decltype ~manifest:t (typestr fn.name)
    ^:: item
      [[%stri let [%p ty], [%p tyo] =
                let ty = [%e mkty ctx args fn.return] in
                Foreign.funptr ty, Foreign.funptr_opt ty
      ]]
      [ val' tyname [%type: [%t typ fn.name] Ctypes.typ];
        val' L.(tyname//"opt") [%type: [%t typ fn.name] option Ctypes.typ];
      ]
