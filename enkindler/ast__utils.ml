
module Aliases = struct
  module L = Name_study
  module H = Ast_helper
  module Exp = H.Exp
  module Pat = H.Pat
  module P = Parsetree
end
open Aliases

type ('a,'b) dual = {p:'a ; e:'b}


let nloc = Location.mknoloc

let typestr n = Fmt.strf "%a" L.pp_type n

let lid s =  Longident.Lident s
let nlid x = nloc(lid x)
let typename n = lid (typestr n)

let varname n = Fmt.strf "%a" L.pp_var n
let ident x = Exp.ident (nloc x)

let pvar s = Pat.var (nloc s)
let ident' x = { p= pvar x; e = ident (lid x) }

let mkconstr x = Fmt.strf "%a" L.pp_constr x

let modname = Fmt.strf "%a" L.pp_module

let (/) x s = Longident.( Ldot(x,s) )

let qn module' x =
  (lid @@modname module') / x

let typ n = H.Typ.constr (nloc @@ typename n) []
let pty n = Ast_helper.Pat.var @@ nloc @@ Fmt.strf "%a" L.pp_type n

let typexp n = [%expr [%e typename n]]
let tyvar n = ident @@ typename n
let var n = let s = varname n in
  { p = Pat.var (nloc s);
    e= Exp.ident (nlid @@ s) }

let pat f x = (f x).p
let ex f x = (f x).e

let (%) f g x = f (g x)
let int = {
  e = Exp.constant % H.Const.int;
  p = Pat.constant % H.Const.int
}

let string s = Ast_helper.(Exp.constant @@ Const.string s)


let norec = Asttypes.Nonrecursive
let tyrec = Asttypes.Recursive

let decltype ?(recflag=tyrec) ?manifest ?kind name =
  H.Str.type_ recflag [H.Type.mk ?kind ?manifest name]

let module_gen name me =
  H.( Str.module_ @@ Mb.mk (nloc @@ modname name) me )

let module' name str = module_gen name (H.Mod.structure str)


let include' me = Ast_helper.(Str.include_ @@ Incl.mk me)
module Me = struct
  let apply f x = H.Mod.(apply (ident @@ nlid f) (ident @@ nlid x))
end

let make_genf name f =
  module_gen name H.Mod.( apply (ident @@ nloc @@ lid f/"Make") @@ structure [])


let variant name constrs =
  decltype ~kind:(P.Ptype_variant constrs) name

let polyvariant name constrs =
  let ty c =
    P.Rtag (c,[],true,[]) in
  let typ = H.Typ.variant (List.map ty constrs) Asttypes.Closed None in
  H.Str.type_ norec [H.Type.mk ~manifest:typ name]

let open' name e = Exp.open_ Asttypes.Fresh (nlid @@ modname name) e
