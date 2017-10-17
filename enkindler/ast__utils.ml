
module Aliases = struct
  module L = Name_study
  module H = Ast_helper
  module Exp = H.Exp
  module Pat = H.Pat
  module P = Parsetree
end
open Aliases
open Ast__item

type ('a,'b) dual = {p:'a ; e:'b}

let nloc = Location.mknoloc


let typestr n = Fmt.strf "%a" L.pp_type n
let val' name ty  = H.Sig.value @@ H.Val.mk (nloc @@ typestr name) ty

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

let typ ?par name =
  let lid =
    match par with
    | Some  m -> lid (modname m) / typestr name
    | None -> typename name in
  H.Typ.constr (nloc @@ lid ) []

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
let type' ?(recflag=tyrec) ty =
  item (H.Str.type_ recflag ty) (H.Sig.type_ recflag ty)

let decltype ?recflag ?manifest ?kind name =
  type' ?recflag [H.Type.mk ?kind ?manifest @@ nloc name]

let module_gen name me =
  let name = nloc @@ modname name in
  item
  H.( Str.module_ @@ Mb.mk name @@ str me )
  H.(Sig.module_ @@ Md.mk name @@ sg me )

let module' name dual = module_gen name @@
  item (H.Mod.structure @@ str dual) (H.Mty.signature @@ sg dual)

let (~:) x = L.simple [x]
let include' me = Ast_helper.(Str.include_ @@ Incl.mk me)
module Me = struct
  let apply f x = H.Mod.(apply (ident @@ nlid f) (ident @@ nlid x))
end

let make_genf ?(suffix="") name f =
  module_gen name @@ item
    H.Mod.( apply (ident @@ nloc @@ lid f/("Make" ^ suffix)) @@ structure [])
    (H.Mty.ident (nloc @@ lid f/("S"^suffix)))

let variant name constrs =
  decltype ~kind:(P.Ptype_variant constrs) name

let polyvariant_type ~closed constrs =
  let ty c =
    P.Rtag (c,[],true,[]) in
  let tyn c = c in
  if not closed then
    H.Typ.variant [] Asttypes.Open (Some (List.map tyn constrs))
  else
    H.Typ.variant (List.map ty constrs) Asttypes.Closed None


let polyvariant name constrs =
  let typ = polyvariant_type ~closed:true constrs in
  type' [H.Type.mk ~manifest:typ name]

let open' name e = Exp.open_ Asttypes.Fresh (nlid @@ modname name) e
