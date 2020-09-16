module B = Lib
module Ty = B.Ty
module U=Utils
module C = Common
module L = Info.Linguistic

open U

let ptr types = function
  | Ty.Option _ -> [%expr None]
  | t ->
    C.coerce ~from:C.(ptr void) ~to':(Type.converter types ~degraded:true t)
      [%expr Ctypes.null]



type t = { test: expression; value: expression }

exception Not_implemented

let bty = Inspect.bty

let from_type types = function
  | Ty.Ptr _ as ty ->
    { value = ptr types ty; test = [%expr Ctypes.is_null] }
  | Ty.Name t ->
    let zero = match Inspect.typeclass t types with
      | Builtin | Prim -> qualify [bty;t] "zero"
      | Typedef ->
        (lid @@ modname @@ tymod t) / "zero"
      | Result -> assert false in
    let zero = ident zero in
    { value = zero ; test = [%expr ((=) [%e zero])] }
  | _ -> raise Not_implemented
