
module Aliases = struct
  module L = Info.Linguistic
  module H = Ast_helper
  module Exp = H.Exp
  module Pat = H.Pat
  module P = Parsetree
end
open Aliases
open Item


let type_module = L.simple ["types"]

type ('a,'b) dual = {p:'a ; e:'b}

let nloc = Location.mknoloc


let typestr n = Fmt.strf "%a" L.pp_type n
let val' name ty  = H.Sig.value @@ H.Val.mk (nloc @@ typestr name) ty

let lid s =  Longident.Lident s
let nlid x = nloc(lid x)


let modname = Fmt.strf "%a" L.pp_module

let qualify l name =
  let open Longident in
  List.fold_left (fun gen elt x -> Ldot(gen elt,x) )
    (fun s -> Lident s)
    (List.map modname l)
    name

let varname n = Fmt.strf "%a" L.pp_var n
let typename n = lid(typestr n)

let varpath ?(par=[]) n = qualify par @@ varname n
let typepath ?(par=[]) n = qualify par @@ typestr n


let ident x = Exp.ident (nloc x)

let pvar s = Pat.var (nloc s)
let ident' x = { p= pvar x; e = ident (lid x) }

let mkconstr x = (Fmt.strf "%a" L.pp_constr x)



let (/) x s = Longident.( Ldot(x,s) )

let qn module' x =
  (lid @@modname module') / x

let (%) f g x = f (g x)


let typ ?par name =
  let lid =
    match par with
    | Some m -> qualify m (typestr name)
    | None -> typename name in
  H.Typ.constr (nloc @@ lid ) []

let pty n = Ast_helper.Pat.var @@ nloc @@ Fmt.strf "%a" L.pp_type n

let any = { p = [%pat? _ ]; e = [%expr ()] }
let typexp n = [%expr [%e typename n]]
let tyvar ?(par=[]) n = ident @@ qualify par (typestr n)
let var n =
  let s = varname n in
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

let module' name me =
  let name = nloc @@ modname name in
  item
  H.( Str.module_ @@ Mb.mk name @@ str me )
  H.(Sig.module_ @@ Md.mk name @@ sg me )

let structure dual =
  item (H.Mod.structure @@ str dual)
    (H.Mty.signature @@ sg dual)

let modtype ?(par=[]) name =
  let s = H.(Mty.ident @@ nloc @@ qualify par @@ modname name ) in
  item s s

let functor' name mty result =
  item
    H.( Mod.functor_ (nloc name) (Some mty) @@ str result )
    H.( Mty.functor_ (nloc name) (Some mty) @@ sg result )

let (~:) x = L.simple [x]
let include' me = Ast_helper.(Str.include_ @@ Incl.mk me)
module Me = struct
  let apply f x = H.Mod.(apply (ident @@ f) (ident @@ nlid x))
end

let make_genf ?(suffix="") name f =
  module' name @@  item
    H.Mod.(apply (ident @@ nloc @@ lid f/("Make" ^ suffix)) @@ structure [])
    H.Mty.(ident (nloc @@ lid f/("S"^suffix)))

let variant name constrs =
  decltype ~kind:(P.Ptype_variant constrs) name


type order = Eq | Lesser | Greater
let polyvariant_type ~order constrs =
  let ty c =
    P.Rtag (c,[],true,[]) in
  match order with
  | Eq -> H.Typ.variant (List.map ty constrs) Asttypes.Closed None
  | Greater -> H.Typ.variant (List.map ty constrs) Asttypes.Open None
  | Lesser -> H.Typ.variant (List.map ty constrs) Asttypes.Closed (Some[])


let polyvariant name constrs =
  let typ = polyvariant_type ~order:Eq constrs in
  type' [H.Type.mk ~manifest:typ name]

let open' name e = Exp.open_ Asttypes.Fresh (nlid @@ modname name) e

let info msg =
  nloc "olivine.info",
  Parsetree.PStr [H.Str.eval @@ H.Exp.constant @@ Parsetree.Pconst_string(msg,None)]

let  (<?>) (exp:Parsetree.expression) msg =
  Format.kasprintf
    (fun msg -> { exp with pexp_attributes = [info msg] }
    ) msg
let (<?:>) ty msg = { ty with Parsetree.ptyp_attributes = [info msg] }
