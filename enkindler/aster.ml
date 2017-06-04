module L = Name_study
module B = Lib_builder
module Ty = Lib_builder.Ty
module T = Lib_builder.T


module H = Ast_helper
module Exp = H.Exp
module Pat = H.Pat
module P = Parsetree

module Aux = struct
  let is_result_name x = L.to_path x = ["result"]
  let is_option_f = function
    | Ty.Simple (_, (Option _ | Const Option _) )
    | Ty.Array_f { index = _, (Option _  | Const Option _ ); _ }
    | Ty.Array_f { array = _, (Option _ | Const Option _); _ }
    | Ty.Record_extension _ -> true
    | _ -> false

  let is_option = function
    | Ty.Option _ -> true
    | _ -> false

  let is_ptr_option = function
    | Ty.Ptr Option _ -> true
    | _ -> false

    let is_void = function
    | Ty.Name {L.main = ["void"]; prefix=[]; postfix = [] }->
      true
    | _ -> false

  let is_result = function
    | Ty.Result _ -> true
    | _ -> false

  let rec find_field_type name = function
    | [] -> None
    | Ty.Simple(n,ty) :: _  when n = name -> Some ty
    | Array_f { index= n, ty ; _ } :: _ when n = name -> Some ty
    | _ :: q -> find_field_type name q

   let find_record tn types =
    match List.assoc tn types with
    | B.Type Ty.Record{ fields; _ } -> fields
    | B.Type ty ->
      Fmt.epr "Path ended with %a@.%!" Ty.pp ty;
      raise @@ Invalid_argument "Non-record path, a least a type"
    | _ ->
      raise @@ Invalid_argument "Non-record path: not even a type"

  let type_path types fields p =
    let rec type_path (types) acc (ty, fields) = function
      | [] -> acc
      | [a] -> (ty,a) :: acc
      | a :: q ->
        match find_field_type a fields with
        | Some (Ty.Const Ptr Name tn| Ptr Name tn) ->
          let fields = find_record tn types in
          type_path types ( (ty, a) :: acc) (Some tn,fields) q
        | Some ty ->
          Fmt.epr "Path ended with %a@.%!" Ty.pp ty;
          raise @@ Invalid_argument "Non-record path"
        | _ -> raise @@ Invalid_argument "Unknown type in path" in
    type_path types [] (None, fields) p

  let rec final_option types fields =
    function
    | [] -> raise (Invalid_argument "Empty type path")
    | [ name ] ->
      begin match find_field_type name fields with
        | Some Ty.Option _ -> true
        | _  -> false
      end
    | name :: q ->
      final_option types (find_record name types) q


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
let ident x = Exp.ident (nloc x)

let modname = Fmt.strf "%a" L.pp_module

let (/) x s = Longident.( Ldot(x,s) )

let qn module' x =
  (lid @@modname module') / x

let typ n = [%type: [%t typename n]]
let pty n = Ast_helper.Pat.var @@ nloc @@ Fmt.strf "%a" L.pp_type n

let typexp n = [%expr [%e typename n]]
let tyvar n = ident @@ typename n
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
  { p = Pat.var (nloc s); e = Exp.ident (nlid s) }

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

let open' name e = Exp.open_ Asttypes.Fresh (nlid @@ modname name) e

let views name =
  let n = var name and no = var L.(name//"opt") in
  let e = open' name [%expr view, view_opt ]
  in [%stri let [%p n.p], [%p no.p] = [%e e]]

let extern_type name =
  decltype ~manifest:(H.Typ.constr (nloc @@ qn name "t")  [])
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
      let v = Pat.var (nloc b) and e = ident (qn name n) in
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
    let x = unique () in
    let m =
      let case c = let s = constr ty c in
        Exp.case (cstr.p s) (string s) in
      Exp.match_ x.e (List.map case constrs) in
    [%stri let [%p pp] = fun ppf [%p x.p] -> Printer.pp_print_string ppf [%e m]]

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

  module M = B.Result.Map

  let find name m =
    try M.find name m with
    | Not_found ->
      Fmt.(pf stderr) "Either.find: not found %a\n%!"
        L.pp_var name;
      List.iter (fun (name,id) -> Fmt.(pf stderr) "%a:%d\n%!"
                    L.pp_var name id)
      @@ M.bindings m;
      raise Not_found

  let view m name constrs =
    let constrs =
      List.map (fun name -> name, T.Abs (find name m)) constrs in
    module' name Enum.[ of_int (Poly,name) constrs;
                        to_int (Poly,name) constrs ]

  let make m (name,ok,error) = match ok, error with
    | [], x | x, [] -> [view m name x]
    | _ ->
      let v, ok_name, error_name = let open Subresult in
        var (composite_nominal ok error), side_name ok, side_name error in
      let conv name = open' name [%expr of_int, to_int] in
      [[%stri let [%p v.p] =
               Vk__result.view ~ok:[%e conv ok_name] ~error:[%e conv error_name]
      ]]
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


module Structured = struct

  type 'field kind =
    | Union: Ty.simple_field kind
    | Record: Ty.field kind

  let sfield (name,typ) =
    (* Note: we could try to simplify further field names,
       but they happen to be quite short in practice *)
    let name = var name and str = varname name and ty = Typexp.make false typ in
    [%stri let [%p name.p] = field t [%e string str] [%e ty] ]

  let field = function
    | Ty.Simple f -> [sfield f]
    | Array_f r -> [sfield r.index; sfield r.array]
    | Record_extension {tag; ptr; _ } -> [sfield tag; sfield ptr]

  let (#.) r x = [%expr Ctypes.getf [%e r] [%e x]]
  let fname x = Exp.ident (nloc @@ lid "Field" / x)
  let get_field f = [%expr record] #. [%expr [%e fname f]]
  let get_field' = get_field%varname
  let mk_array n x = [%expr Ctypes.CArray.from_ptr [%e n] [%e x]]


  let rec get_path f = function
    | [] -> raise @@ Invalid_argument "Printers.pp_path: empty path"
    | [None, a] -> f a
    | (Some ty, a) :: q ->
      [%expr Ctypes.(!@) [%e get_path f q]]
      #. (ident (lid  (modname ty) / "Fields" / (varname a)))
    | (None, _) :: _ -> raise @@
      Invalid_argument "Printers.pp_path: p artially type type path"

  let array_index types fields = function
    | Ty.Lit n -> int.e n
    | Path p ->
      let pty= Aux.type_path types fields p in
      get_path (fun x -> (var x).e) pty
    | _ -> assert false

  let unopt = function
    | Ty.Option ty -> ty
    | ty -> ty

  let rec to_int var = function
    | Ty.Ptr t -> to_int [%expr Ctypes.(!@) [%e var]] t
    | Option t -> to_int [%expr unwrap [%e var]] t
    | Name t when L.to_path t = ["uint32";"t"] -> var
    | Name t -> [%expr [%e ident @@ qn t "to_int"] var ]
    | _ -> raise @@ Invalid_argument "Invalid Printers.Fn.to_int ty"

  let lens types fields (out,field) =
    let u = unique () in
    let def x = [%stri let [%p out] = fun [%p u.p] -> [%e x] ] in
    def begin match field with
      | Ty.Array_f {index=i,ty; array = a, tya} as f when Aux.is_option_f f ->
        let index =  to_int (string "n") (unopt ty) in
        [%expr match [%e get_field' i], [%e get_field' a] with
          | [%p if Aux.is_option ty then [%pat? Some n] else [%pat? n]],
            [%p if Aux.is_option tya then [%pat? Some a] else [%pat? a]] ->
            Some [%e mk_array index [%expr a] ]
          | _ -> None
        ]
      | Array_f {array= x, tya; index = n, ty } ->
        let mk = mk_array (to_int (var n).e ty) in
        let xv = var x in
        let body = if Aux.is_option tya then
            [%expr may (fun x -> [%e mk [%expr x]]) vx.d]
          else mk xv.e in
        [%expr let [%p (var n).p] = [%e get_field "n"]
          and [%p xv.p] = [%e get_field "x"] in [%e body] ]
      | Record_extension _ -> [%expr assert false]
      | Simple(n, (Option Array(Some Path p, _)| Array(Some Path p,_) as t)) ->
        let p' = Aux.type_path types fields p in
        let a = mk_array [%expr i] [%expr a] in
        let body = let aopt = Aux.is_option t
          and opt = Aux.final_option types fields p in
          if opt && aopt then
            [%expr maybe (may (fun i a -> [%e a]) i) a]
          else if opt then
            [%expr may (fun i -> [%e a]) i ]
          else if aopt then
            [%expr may (fun a -> [%e a]) a ]
          else a in
        [%expr let a = [%e get_field' n]
          and i = [%e get_path get_field' p'] in [%e body] ]
      | Simple(name,_) -> get_field (varname name)
    end

  let rec printer types =
    let printer = printer types in
    let pp module' = ident @@ (qn module') "pp" in
    let abstract = [%expr pp_abstract] in
    function
    | Ty.Name t ->
      begin match List.assoc t types with
        | exception Not_found -> pp t
        | B.Type (Ty.FunPtr _ |Union _ ) -> abstract
        | B.Type Ty.Bitfields _ -> pp (Bitset.set_name t)
        | _ -> pp t end
    | Array(_, Name t) when L.to_path t = ["char"] ->[%expr pp_string]
    | Array(Some Math_expr,_t) -> (*FIXME*) abstract
    | Array(_,t) -> [%expr pp_array [%e printer t]]
    | Option t -> [%expr pp_opt [%e printer t]]
    | Const t -> printer t
    | Ptr t when Aux.is_void t -> [%expr pp_addr]
    | Ptr t -> [%expr pp_ptr [%e printer t]]
    | String -> [%expr pp_string]
    | ty -> Fmt.epr "Not implemented: %a@." Ty.pp ty;
      raise @@ Invalid_argument "Printer not implemented"

  let seq = listr (fun x y -> [%expr [%e x]; [%e y]])

  let pp types fields =
    let u = unique () in
    let with_pf x = [%expr let pf ppf = Printer.fprintf ppf in [%e x]  ] in
    let pp_f (name,ty) =
      let fmt = string @@ varname name ^ "=%a" in
      [%expr pf ppf [%e fmt] [%e printer types ty] [%e u.e]] in
    let def x =
      [%stri let pp ppf = fun [%p u.p] ->
          let pf ppf = Printer.fprintf ppf in
          pf ppf "@[{@ "; [%e with_pf x] ] in
    let pp_field (field:Ty.field) = match field with
      | Record_extension _ -> [%expr pf ppf "ext=⟨unsupported⟩"]
      | Array_f { array=name, (Ty.Ptr ty| Const Ptr ty | Array(_,ty));
                  index=_,tyi  } ->
        let opt x = if Aux.is_option tyi then Ty.Option x else x in
        pp_f (name, opt @@ Array(None, ty))
      | Array_f { array=name, Option(Array(_,ty)|Ptr ty| Const Ptr ty); _} ->
        pp_f (name, Option (Array(None, ty)))
      | Array_f { array=_,ty; _ } ->
        Fmt.epr "Structured.pp: %a@." Ty.pp ty; assert false
      | Simple f -> pp_f f in
    def @@ seq pp_field  fields [%expr pf ppf "@ }@]"]

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
