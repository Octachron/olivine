module L = Name_study
module Ty = Lib_builder.Ty
module T = Lib_builder.T


module H = Ast_helper
module Exp = H.Exp
module Pat = H.Pat
module P = Parsetree

module Aux = struct
  let is_result_name x = L.to_path x = ["result"]
end

let list merge map acc =
  List.fold_left (fun acc x -> merge acc @@ map x) acc

let listr merge map l last =
  List.fold_right (fun x acc -> merge (map x) acc) l last

type ('a,'b) dual = {p:'a ; e:'b}

let nloc = Location.mknoloc

let typestr n = Fmt.strf "%a" L.pp_type n

let lid s =  Longident.Lident s
let nlid x = nloc(lid x)
let typename n = lid (typestr n)

let varname n = Fmt.strf "%a" L.pp_var n

let modname = Fmt.strf "%a" L.pp_module

let (/) x s = Longident.( Ldot(x,s) )

let qn module' x =
  (nloc @@ (lid @@modname module')/ x)


let typ n = [%type: [%t typename n]]

let pty n = Ast_helper.Pat.var @@ nloc @@ Fmt.strf "%a" L.pp_type n

let typexp n = [%expr [%e typename n]]
let tyvar n = Ast_helper.Exp.ident @@ nloc @@ typename n
let var n = let s = varname n in
  { p = Pat.var (nloc s);
    e= Exp.ident (nlid @@ s) }

let (%) f g x = f (g x)
let int = {
  e = Exp.constant % H.Const.int;
  p = Pat.constant % H.Const.int
}

let string s = Ast_helper.(Exp.constant @@ Const.string s)

let counter = ref 0
let unique () =
  incr counter;
  let s =  "x'" ^ string_of_int !counter ^ "'" in
  Pat.var (nloc s), Exp.ident (nlid s)


let norec = Asttypes.Nonrecursive
let tyrec = Asttypes.Recursive

let decltype ?(recflag=tyrec) ?manifest ?kind name =
  H.Str.type_ recflag [H.Type.mk ?kind ?manifest name]

let module_gen name me =
  H.( Str.module_ @@ Mb.mk (nloc @@ modname name) me )

let module' name str = module_gen name (H.Mod.structure str)
let make_genf name f =
  module_gen name H.Mod.( apply (ident @@ nloc @@ lid f/"Make") @@ structure [])

let variant name constrs =
  decltype ~kind:(P.Ptype_variant constrs) name

let polyvariant name constrs =
  let ty c =
    P.Rtag (c,[],true,[]) in
  let typ = H.Typ.variant (List.map ty constrs) Asttypes.Closed None in
  H.Str.type_ norec [H.Type.mk ~manifest:typ name]

let views name =
  let n = var name and no = var L.(name//"opt") in
  let e = Exp.open_ Asttypes.Fresh (nlid @@ modname name) [%expr view, view_opt ]
  in [%stri let [%p n.p], [%p no.p] = [%e e]]

let extern_type name =
  decltype ~manifest:(H.Typ.constr (qn name "t")  [])
    (nloc @@ typestr name)

module Bitset = struct

  let bit_name name =
    let rec bitname = function
      | "flags" :: q ->
        "bits" :: "flag" :: q
      | [] ->
        raise @@ Invalid_argument "bitname []"
      | a :: q -> a :: bitname q in
    L.{ name with postfix = bitname name.postfix }

  let set_name name =
    let rec rename = function
      |  "bits" :: "flag" :: q ->
        "flags" :: q
      | [] ->
        raise @@ Invalid_argument "empty bitset name []"
      | a :: q -> a :: rename q in
    L.{ name with postfix = rename name.postfix }

  let value_name set_name name =
    L.remove_context set_name name

  let field_name set_name name = let open L in
    let context =
      { set_name with postfix = set_name.postfix @ [ "bit" ]  } in
    remove_context context name

  let named namer f set_name (name,value) =
    let v = var @@ namer set_name name in
    [%stri let [%p v.p] = [%e f] [%e int.e value]]

  let field = named field_name [%expr make_index]
  let value = named value_name [%expr of_int]

  let values set_name (fields,values) =
    List.map (field set_name) fields
    @ List.map (value set_name) values

  let pp set (fields,_) =
    let field (name,_) =
      let name = field_name set name in
      [%expr [%e (var name).e], [%e string (varname name)]] in
    let l = listr (fun x l -> [%expr [%e x] :: [%e l] ]) field fields [%expr []] in
    [%stri let pp x = pp_tags [%e l] x]

  let resume bitname name =
    let index_view b n =
      let v = Pat.var (nloc b) and e = Exp.ident (qn name n) in
      [%stri let [%p v] = [%e e ]] in
    [extern_type name; views name;
     index_view bitname "index_view";
     index_view (bitname^"_opt") "index_view_opt" ]

  let make_extended (bitname, fields) =
  let name = set_name bitname in
  let core_name = let open L in
    { name with postfix = List.filter (fun x -> x <> "flags") name.postfix }
  in
  module' name (
    [%stri include Bitset.Make()]
    :: values core_name fields
    @ [pp core_name fields]
  )
  :: resume (varname bitname) name

  let make (name,opt) =
    let bitname = bit_name name in
    match opt with
    | Some _ -> []
    | None ->
      make_genf name "Bitset"
      :: resume (varname bitname) name

end

module Handle = struct
  let make name =
    [ make_genf name "Handle"; extern_type name; views name ]
end

module Enum = struct

  type implementation = Std | Poly

  let constr (_,name_0) (c, _) =
    let name = L.remove_context name_0 c in
    let name = if name = L.mu then name_0 else name in
    Fmt.strf "%a" L.pp_constr name

  let def_std name constrs =
    let constr c = let n = constr (Std,name) c in
      H.Type.constructor (nloc n) in
    variant (nloc "t") @@ List.map constr constrs

  let def_poly name constrs =
    let constr c = constr (Poly,name) c in
    polyvariant (nloc "t") @@ List.map constr constrs

  let def (impl,name) = match impl with
    | Std -> def_std name
    | Poly -> def_poly name

  let case pair ty (_, p as c) =
    match p with
    | T.Abs n ->  pair (constr ty c) n
    | _ -> assert false

  let construct (impl,_) = match impl with
    | Std -> { p = (fun s -> Pat.construct (nlid s) None)
             ; e = (fun s -> Exp.construct (nlid s) None) }
    | Poly -> { p = (fun s -> Pat.variant s None);
                e = (fun s -> Exp.variant s None) }


  let conv ?(cases=[]) f params constrs =
     Exp.function_ (List.map (case f params) constrs @ cases)

  let to_int ty constrs =
    let c = construct ty in
    let f =
      conv (fun n d -> Exp.case (c.p n) (int.e d)) ty constrs in
    [%stri let to_int = [%e f] ]

  let of_int ty constrs =
    let c = construct ty in
    let app n d = H.Exp.case (int.p d) (c.e n) in
    let cases = [Exp.case (Pat.any ()) [%expr assert false]] in
    let f = conv ~cases app ty constrs in
    [%stri let of_int = [%e f] ]

  let pp pre ty constrs =
    let cstr = construct ty in
    let pp = Pat.var @@ nloc @@ pre ^ "pp" in
    let x', x = unique () in
    let m =
      let case c = let s = constr ty c in
        Exp.case (cstr.p s) (string s) in
      Exp.match_ x (List.map case constrs) in
    [%stri let [%p pp] = fun ppf [%p x'] -> Printer.pp_print_string ppf [%e m]]

  let view =
    [%stri let view = Ctypes.view ~write:to_int ~read:of_int int]
  let view_result =
    [%stri let view = Vk__result.view  ~ok:(of_int,to_int) ~error:(of_int,to_int)]
  let pp_result =
    [%stri let pp = Vk__result.pp raw_pp]

  let view_opt =
    [%stri let view_opt =
             let read x = if x = max_int then None else Some(of_int x) in
             let write = function None -> max_int | Some x -> to_int x in
             Ctypes.view ~read ~write int
    ]


  let make impl (name,constrs) =
    let is_result = Aux.is_result_name name in
    let pre = if is_result then "raw_" else "" in
    let ty = impl, name in
    let str =
      (List.map (fun f -> f ty constrs)
         [def; to_int;of_int; pp pre ])
      @ (if is_result then [pp_result; view_result] else [view])
      @ [view_opt] in
    let m =
      module' name str in
    [m; views name ; extern_type name ]

end

module Result = struct

  let expr (ok,errors) =
    tyvar @@ Subresult.composite_nominal ok errors
end

module Typexp = struct
    let rec make degraded x =
      let make = make degraded in
      match x with
      | Ty.Const t -> make t
      | Name n -> tyvar n
      | Ptr Name n | Ptr Const Name n ->
        [%expr ptr [%e tyvar n]]
      | Ptr ty -> [%expr ptr [%e make ty] ]
      | Option Name n -> [%expr ptr [%e tyvar L.(n//"opt")]]
      | Option (Ptr typ) -> [%expr ptr_opt [%e make typ] ]
      | Option Array (_,t) -> [%expr ptr_opt [%e make t]]
      | Option String -> [%expr string_opt]
      | Option t -> Fmt.epr "Not implemented: option %a@." Ty.pp t; exit 2
      | String -> [%expr string]
      | Array (Some Const n ,typ) -> [%expr array [%e tyvar n] [%e make typ]]
      | Array (Some (Lit n) ,typ) when not degraded ->
        [%expr array [%e int.e n] [%e make typ]]
      | Array (_,typ) -> make (Ty.Ptr typ)
      | Enum _ | Record _ | Union _ | Bitset _ | Bitfields _
      | Handle _  ->
        failwith "Anonymous type"
      | Result {ok;bad} ->
        Result.expr (ok,bad)
      | Record_extensions _ -> [%expr ptr void]
      (* ^FIXME^?: better typing? *)
      | FunPtr _ ->
        failwith "Not_implemented: funptr"


end


module Funptr = struct

  let mkty (fn:Ty.fn) =
    let ret  = Typexp.make true fn.return in
    listr (fun l r -> [%expr[%e l] @-> [%e r] ]) (Typexp.make true)
      (List.map snd @@ Ty.flatten_fn_fields fn.args)
      [%expr returning [%e ret]]

  let make (tyname, (fn:Ty.fn)) =
    let ty = pty tyname and tyo = pty L.(tyname//"opt")in
    match List.map snd @@ Ty.flatten_fn_fields fn.args with
    | [] -> [%stri let [%p ty] = ptr void ]
    | _ ->
      [%stri let [%p ty], [%p tyo] =
               let ty = [%e mkty fn] in
               Foreign.funptr ty, Foreign.funptr_opt ty
      ]

end
