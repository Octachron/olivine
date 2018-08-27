module Aliases= struct
  module L = Info.Linguistic
  module H = Ast_helper
  module Exp = H.Exp
  module Pat = H.Pat
  module P = Parsetree
  module C = Common
end
open Aliases
open Item
open Utils

let stype = ident' "stype__generated"
let pnext = ident' "pnext__generated"

let name root_name =  L.(root_name//"ext")

let def (typename,exts) =
  let name = typestr @@ name typename  in
  let ty t = typ t in
  let ext e = H.Te.decl ~args:(Pcstr_tuple [ty e]) (nloc @@ mkconstr e) in
  let exts = H.Te.decl (nloc "No_extension") :: List.map ext exts in
  let te = H.Te.mk (nlid name) exts in
  let extend = item (H.Str.type_extension te) (H.Sig.type_extension te)  in
  let decl =
    type' [H.Type.mk ~kind:P.Ptype_open (nloc name)] in
  item [%stri exception Unknown_record_extension]
    [%sigi: exception Unknown_record_extension]
^:: decl
^:: extend
^:: nil

let constr =
  let n = nlid % mkconstr in
  let p x var = Pat.construct (n x) (Some var)
  and e x var = Exp.construct (n x) (Some var) in
  {e; p}

let flag x = qn (Utils.tymod (~:"Structure_type")) (mkconstr x)
let str = ident % flag
let strp x = Pat.construct (nloc @@ flag x) None

let typext = ident % typename

let split tag ptr (name,exts) input =
  let v = ident' "x" in
  let case x = Exp.case (constr.p x v.p)
      [%expr [%e str x] ,
             [%e C.coerce (C.ptr @@ typext x) C.(ptr void) (C.addr v.e)]] in
  let noext_case =
    let null = if Inspect.is_option (snd ptr) then
        [%expr None] else [%expr Ctypes.null] in
    Exp.case [%pat? No_extension] [%expr [%e str name], [%e null] ] in
  let exn = Exp.case [%pat? _ ] [%expr raise Unknown_record_extension ] in
  let cases = noext_case :: (List.map case exts) @ [exn] in
  Exp.match_ input cases

let merge (name,exts) ~tag ~data =
  let case ext = Exp.case (strp ext)
      C.(constr.e ext @@ (!@) @@ coerce (ptr void) (ptr @@ typext ext) data ) in
  let noext = Exp.case (strp name) [%expr No_extension] in
  let exn = Exp.case [%pat? _] [%expr raise Unknown_record_extension ] in
  let cases = noext :: (List.map case exts) @ [exn] in
  Exp.match_ tag cases
