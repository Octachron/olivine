
module Aliases= struct
  module L = Name_study
  module B = Lib_builder
  module Ty = Lib_builder.Ty
  module T = Lib_builder.T


  module H = Ast_helper
  module Exp = H.Exp
  module Pat = H.Pat
  module P = Parsetree
end
open Aliases
open Ast__utils


(*   let debug fmt = Fmt.epr ( "Debug:"^^ fmt ^^ "@.%!") *)

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

  let is_char = function
    | Ty.Name t -> L.to_path t = ["char"]
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

  let is_extension =
    function
    | Ty.Record_extension _ -> true
    | _ -> false

  let record_extension fields =
    match List.find is_extension fields with
    | exception Not_found -> None
    | Ty.Record_extension {exts;_} ->  Some exts
    | _ -> None

  let rec find_field_type name = function
    | [] -> None
    | Ty.Simple(n,ty) :: _  when n = name -> Some ty
    | Array_f { index= n, ty ; _ } :: _ when n = name -> Some ty
    | _ :: q -> find_field_type name q

   let find_record tn types =
    match B.find_type tn types with
    | Some Ty.Record{ fields; _ } -> fields
    | Some ty ->
      Fmt.epr "Path ended with %a@.%!" Ty.pp ty;
      raise @@ Invalid_argument "Non-record path, a least a type"
    | None ->
      raise @@ Invalid_argument "Non-record path: not even a type"

   let is_record types tn =
     match B.find_type tn types with
     | Some Ty.Record _ -> true
     | _ -> false

  let type_path types fields p =
    let rec type_path (types) acc (ty, fields) = function
      | [] -> acc
      | [a] -> (ty,a) :: acc
      | a :: q ->
        match find_field_type a fields with
        | Some (Ty.Const Ptr Name tn| Ptr Name tn|Name tn as tyo) ->
          let direct = match tyo with Name _ -> true | _ -> false in
          let fields = find_record tn types in
          type_path types ((ty, a) :: acc) (Some (direct,tn),fields) q
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

  let to_fields = List.map(fun f -> f.Ty.field)

end

let not_implemented fmt =
  Format.kasprintf (fun s -> raise (Invalid_argument s)) fmt

let list merge map acc =
  List.fold_left (fun acc x -> merge acc @@ map x) acc

let listr merge map l last =
  List.fold_right (fun x acc -> merge (map x) acc) l last


module M = Enkindler_common.StringMap
let dict : int M.t ref = ref M.empty
let unique str =
  let c = match M.find_opt str !dict with Some x -> x | None -> 1 in
  dict := M.add str (1+c) !dict;
  let s =  str ^"'" ^ string_of_int c in
  { p = Pat.var (nloc s); e = Exp.ident (nlid s) }

let reset_uid () = dict := M.empty

let coerce ~from ~to' value = [%expr Ctypes.coerce [%e from] [%e to'] [%e value]]
let ptr x = [%expr Ctypes.ptr [%e x] ]
let void = [%expr void]
let addr x = [%expr Ctypes.addr [%e x] ]
let (!@) x = [%expr !@[%e x]]

let views name =
  let n = var name and no = L.(name//"opt") in
  let e = open' name [%expr view, view_opt ] in
  let t = typ ~par:name ~:"t" in
  item
  [[%stri let [%p n.p], [%p pat var no] = [%e e]]]
  [ val' name [%type: [%t t] Ctypes.typ]; val' no [%type: [%t t] option Ctypes.typ]]

let extern_type name =
  decltype ~manifest:(H.Typ.constr (nloc @@ qn name "t")  [])
    (typestr name)

let wrap_opt ty v =
  if Aux.is_option ty then
    [%expr Some [%e v]]
  else
    v


let unwrap_opt_ty = function
  | Ty.Option ty -> ty
  | ty -> ty

let repr_name = function
  | Ty.Array_f { array = a, _ ; _ } -> (varname a)
  | Simple(n, _ ) -> varname n
  | Record_extension _ -> "ext"

let index_name f = L.(f//"size'")

let mkfun fields =
  let label f name =
    if Aux.is_option_f f then
      Asttypes.Optional name
    else
      Asttypes.Labelled name in
  let add_arg (k,m) =
    let def  ?opt f p body = k @@ Exp.fun_ (label f @@ repr_name f) opt p body in
    let vars = List.fold_left (fun m (k,x) -> M.add k x m) m in
    function
    | Ty.Record_extension _ as f ->
      let u = unique "ext" and opt = [%expr No_extension] in
      def ~opt f u.p, vars ["ext", u]
    | Array_f { array= n, _; index = i, _  } as f ->
      let u = unique (varname n) and v = unique (varname i) in
      def f u.p, vars [varname n, u; varname i, v ]
    | Simple(n, Array _) as f ->
      let u = unique (varname n) and v = unique "size" in
      def f u.p, vars [varname n, u; varname @@ index_name n, v]
    | Ty.Simple(n,_)  as f ->
      let u = unique (varname n) in
      def f u.p, vars [varname n, u] in
  let terminator =
    if List.exists Aux.is_option_f fields || List.length fields = 0
    then fun body -> [%expr fun () -> [%e body]]
    else fun x -> x in
  let f, m = List.fold_left add_arg ((fun x->x), M.empty) fields in
  f % terminator, m


  let regularize types ty exp= match ty with
    | Ty.Ptr Name t | Const Ptr Name t when Aux.is_record types t ->
      addr exp
    | Option Ptr Name _ -> [%expr may [%e addr exp] ]
    | _ ->  exp


  let regularize_fields types fields =
    let reg f =
      match f.Ty.field with
      | Simple(n, (Ptr Name t| Const Ptr Name t))
        when Aux.is_record types t ->
        { f with field = Simple(n, Name t) }
      | Simple(n, Option Ptr Name t) when Aux.is_record types t ->
        { f with field = Simple(n, Option (Name t)) }
      | _ -> f
    in
    List.map reg fields

let annotate fmt =
  Fmt.kstrf (fun s e -> Exp.attr e @@
              (nloc "debug", P.PStr [H.Str.eval @@ string s ]))
      fmt

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

  let named ty namer f set_name (name,value) =
    let n = namer set_name name in
    let v = var n in
    item [%stri let [%p v.p] = [%e f] [%e int.e value]]
    (val' n ty)
  let field = named [%type: index] field_name [%expr make_index]
  let value = named [%type: 'a set]  value_name [%expr of_int]

  let values set_name (fields,values) =
    imap (field set_name) fields
    @* imap (value set_name) values

  let pp set (fields,_) =
    let field (name,_) =
      let name = field_name set name in
      [%expr [%e ex var name], [%e string (varname name)]] in
    let l = listr (fun x l -> [%expr [%e x] :: [%e l] ]) field fields [%expr []] in
    item [%stri let pp x = pp_tags [%e l] x]
      [%sigi: val pp: Format.formatter -> 'a t -> unit]

  let resume bitname name =
    let index_view b ty n =
      let v = Pat.var (nloc b) and e = ident (qn name n) in
      item [%stri let [%p v] = [%e e ]]
        (val' ~:b [%type: [%t ty] Ctypeq.typ ]) in
    let ty = typ ~par:name ~:"index" in
    extern_type name ^:: views name
    @* index_view bitname ty "index_view"
    ^:: index_view (bitname^"_opt") [%type: [%t ty] option] "index_view_opt"
    ^:: nil

  let make_extended (bitname, fields) =
  let name = set_name bitname in
  let core_name = let open L in
    { name with postfix = List.filter (fun x -> x <> "flags") name.postfix }
  in
  let values = values core_name fields in
  module' name (
    item [%stri include Bitset.Make()] [%sigi: include Bitset.S ]
    ^:: values
    @* pp core_name fields ^:: nil
  )
  ^:: resume (varname bitname) name

  let make (name,opt) =
    let bitname = bit_name name in
    match opt with
    | Some _ -> nil
    | None ->
      make_genf name "Bitset"
      ^:: resume (varname bitname) name

end

module Handle = struct
  let make name =
    make_genf name "Handle" ^:: extern_type name ^:: views name
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
    variant "t" @@ List.map constr constrs

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
    item [%stri let to_int = [%e f] ]
      (val' ~:"to_int" [%type: t -> int])

  let of_int ty constrs =
    let c = construct ty in
    let app n d = H.Exp.case (int.p d) (c.e n) in
    let cases = [Exp.case (Pat.any ()) [%expr assert false]] in
    let f = conv ~cases app ty constrs in
    let sigi = val' ~:"of_int" [%type: int -> t] in
    let stri = [%stri let of_int = [%e f] ] in
    item stri sigi

  let pp pre ty constrs =
    let cstr = construct ty in
    let pp = Pat.var @@ nloc @@ pre ^ "pp" in
    let x = unique "x" in
    let m =
      let case c = let s = constr ty c in
        Exp.case (cstr.p s) (string s) in
      Exp.match_ x.e (List.map case constrs) in
    item
      [%stri let [%p pp] = fun ppf [%p x.p] -> Printer.pp_print_string ppf [%e m]]
      (val' ~:(pre ^ "pp") [%type: Format.formatter -> t -> unit])

  let view =
    item [%stri let view = Ctypes.view ~write:to_int ~read:of_int int]
      (val' ~:"view" [%type: t Ctypes.typ])

  let view_result =
    item
      [%stri let view = Vk__result.view  ~ok:(of_int,to_int) ~error:(of_int,to_int)]
    (val' ~:"view" [%type: (t,t) result Ctypes.typ])

  let pp_result =
    item [%stri let pp = Vk__result.pp raw_pp]
      (val' ~:"pp" [%type: Format.formatter -> (t,t) result -> unit])

  let view_opt =
    item
      [%stri let view_opt =
               let read x = if x = max_int then None else Some(of_int x) in
               let write = function None -> max_int | Some x -> to_int x in
               Ctypes.view ~read ~write int
      ]
      (val' ~:"view_opt" [%type: t option Ctypes.typ])

  let make impl (name,constrs) =
    let is_result = Aux.is_result_name name in
    let pre = if is_result then "raw_" else "" in
    let ty = impl, name in
    let str =
      def ty constrs
      ^:: to_int ty constrs
      ^:: of_int ty constrs
      ^:: pp pre ty constrs
      ^:: (if is_result then pp_result ^:: view_result ^:: nil
          else view ^:: nil )
      @* view_opt ^:: nil in
    let m =
      module' name str in
    m ^:: views name @* extern_type name ^:: nil

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
    module' name @@
    Enum.( of_int (Poly,name) constrs ^::
           to_int (Poly,name) constrs ^:: nil
         )

  let make m (name,ok,error) = match ok, error with
    | [], x | x, [] -> (view m name x) ^:: nil
    | _ ->
      let v, ok_name, error_name = let open Subresult in
        (composite_nominal ok error), side_name ok, side_name error in
      let ty x = polyvariant_type @@ List.map typestr x in
      let conv name = open' name [%expr of_int, to_int] in
      item [%stri let [%p pat var v] =
               Vk__result.view ~ok:[%e conv ok_name] ~error:[%e conv error_name]
      ]
        (val' v
            [%type: ([%t ty ok], [%t ty error]) result ]
        ) ^:: nil

end

module Record_extension = struct

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

  let flag x = (lid "Structure_type") /  mkconstr x
  let str = ident % flag
  let strp x = Pat.construct (nloc @@ flag x) None

  let typext = ident % typename

  let split (name,exts) input =
    let v = ident' "x" in
    let case x = Exp.case (constr.p x v.p)
        [%expr [%e str x] ,
               [%e coerce (ptr @@ typext x) (ptr void) (addr v.e)]] in
    let noext_case =
      Exp.case [%pat? No_extension] [%expr [%e str name], Ctypes.null ] in
    let exn = Exp.case [%pat? _ ] [%expr raise Unknown_record_extension ] in
    let cases = noext_case :: (List.map case exts) @ [exn] in
    Exp.match_ input cases

  let merge (name,exts) ~tag ~data =
    let case ext = Exp.case (strp ext)
        (constr.e ext @@ (!@) @@ coerce (ptr void) (ptr @@ typext ext) data ) in
    let noext = Exp.case (strp name) [%expr No_extension] in
    let exn = Exp.case [%pat? _] [%expr raise Unknown_record_extension ] in
    let cases = noext :: (List.map case exts) @ [exn] in
    Exp.match_ tag cases


end

module Type = struct

  let rec converter degraded x =
      let make = converter degraded in
      match x with
      | Ty.Const t -> make t
      | Name n -> tyvar n
      | Ptr Name n | Ptr Const Name n ->
        [%expr ptr [%e tyvar n]]
      | Ptr ty -> [%expr ptr [%e make ty] ]
      | Option Name n -> tyvar L.(n//"opt")
      | Option (Ptr typ) -> [%expr ptr_opt [%e make typ] ]
      | Option Array (Some Const n ,typ) ->
        [%expr array_opt [%e (var n).e] [%e make typ] ]
      | Option Array (Some (Lit n) ,typ) when not degraded ->
        [%expr array_opt [%e int.e n ] [%e make typ ]]
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

  let rec mk = function
      | Ty.Const t -> mk t
      | Name n -> [%type: [%t typ n]]
      | Ptr ty -> [%type: [%t mk ty] Ctypes.ptr ]
      | Option ty -> [%type: [%t mk ty] option ]
      | String -> [%type: string]
      | Array (Some (Const _ |Lit _ |Path _ ) , ty) ->
        [%type: [%t mk ty] Ctypes.CArray.t ]
      | Result {ok;bad} ->
        let ok = polyvariant_type @@ List.map mkconstr ok in
        let bad = polyvariant_type @@ List.map mkconstr bad in
        [%type: ([%t ok], [%t  bad]) result ]
      | Record_extensions _ -> [%type: void Ctypes.ptr ]
      (* ^FIXME^?: better typing? *)
      | Array (_,ty) -> [%type: [%t mk ty] Ctypes.ptr]
      | FunPtr _ -> not_implemented "funptr type"
      | Enum _ | Record _ | Union _ | Bitset _ | Bitfields _
      | Handle _  ->
        failwith "Anonymous type"

    let fn ?(with_label=false) typename fields ret =
      let (->>) (l,x) r = H.Typ.arrow l x r in
      let label n f =
        if not with_label then Asttypes.Nolabel
        else if Aux.is_option_f f then
          Asttypes.Optional (varname n)
        else
          Labelled (varname n) in
      let arg f = match f with
        | Ty.Array_f { array=n, ty; _ } ->
          label n f, mk ty
        | Simple(n,ty) as f ->
          label n f , mk ty
        | Record_extension _ ->
          label ~:"ext" f ,  typ (Record_extension.name typename) in
      let ret = if List.exists Aux.is_option_f fields then
          (Nolabel, [%type: unit] ) ->> ret
        else
          ret in
      List.fold_right (fun field f -> arg field ->> f ) fields ret


end


module Structured = struct

  type 'field kind =
    | Union: Ty.simple_field kind
    | Record: Ty.field kind

  let sfield (name,t) =
    (* Note: we could try to simplify further field names,
       but they happen to be quite short in practice *)
    let str = varname name and conv = Type.converter false t in
    let ty = Type.mk t in
    item
      [%stri let [%p pat var name] = field t [%e string str] [%e conv] ]
      (val' name [%type: ([%t ty] , t) Ctypes.field])

  let field = function
    | Ty.Simple f -> sfield f ^:: nil
    | Array_f r -> sfield r.index ^:: sfield r.array ^:: nil
    | Record_extension {tag; ptr; _ } ->
      sfield tag ^:: sfield ptr ^:: nil

  let (#.) r x = [%expr Ctypes.getf [%e r] [%e x]]
  let fname x = Exp.ident (nloc @@ lid "Fields" / x)
  let get_field r f = r #. [%expr [%e fname f]]
  let get_field' r = get_field r % varname
  let mk_array n x = [%expr Ctypes.CArray.from_ptr [%e x] [%e n]]

  let rec get_path f = function
    | [] -> raise @@ Invalid_argument "Printers.pp_path: empty path"
    | [None, a] -> f a
    | (Some (direct, ty), a) :: q ->
      (if direct then get_path f q else [%expr Ctypes.(!@) [%e get_path f q]])
      #. (ident (lid  (modname ty) / "Fields" / (varname a)))
    | (None, _) :: _ -> raise @@
      Invalid_argument "Printers.pp_path: partially type type path"

  let array_index types fields = function
    | Ty.Lit n -> int.e n
    | Path p ->
      let pty= Aux.type_path types fields p in
      get_path (ex var) pty
    | _ -> assert false

  let rec int_of_ty var = function
    | Ty.Ptr t -> int_of_ty [%expr Ctypes.(!@) [%e var]] t
    | Option t -> int_of_ty [%expr unwrap [%e var]] t
    | Name t when L.to_path t = ["uint32";"t"] -> var
    | Name t -> [%expr [%e ident @@ qn t "to_int"][%e var] ]
    | _ -> raise @@ Invalid_argument "Invalid Printers.Fn.to_int ty"


  let start x = [%expr Ctypes.CArray.start [%e x]]

  let inner = ~:"Fields"

  let setf r f value =
    [%expr Ctypes.setf [%e r] [%e ident(qn inner @@ f)] [%e value] ]

  let union (n,ty) =
    reset_uid ();
    let u = unique "x" and r = unique "res" in
    item [%stri let [%p pat var n] = fun [%p u.p] ->
        let [%p r.p] = Ctypes.make t in
        [%e setf r.e (varname n) u.e ]; [%e r.e]
    ]
      (val' n [%type: t -> [%t Type.mk ty] ])

  let type_field = function
    | Ty.Simple(_,ty) -> Type.mk ty
    | Array_f { array = _, ty; _ } -> Type.mk ty
    | Record_extension _ -> typ ~:"ext"

  let getter typename types fields (out_name,field) =
    let u = unique "record" and out = var out_name in
    let get_field' = get_field' u.e and get_field = get_field u.e in
    let def x =
      item
        [%stri let [%p out.p] = fun [%p u.p] -> [%e x] ]
        (val' out_name @@ type_field field) in
    def begin match field with
      | Ty.Array_f {index=i,ty; array = a, tya} as f when Aux.is_option_f f ->
        let index = int_of_ty (ex ident' "n") (unwrap_opt_ty ty) in
        [%expr match [%e get_field' i], [%e get_field' a] with
          | [%p if Aux.is_option ty then [%pat? Some n] else [%pat? n]],
            [%p if Aux.is_option tya then [%pat? Some a] else [%pat? a]] ->
            Some [%e mk_array index [%expr a] ]
          | _ -> None
        ]
      | Array_f {array= x, tya; index = n, ty } ->
        let mk = mk_array (int_of_ty (ex var n) ty) in
        let xv = var x in
        let body = if Aux.is_option tya then
            [%expr may (fun x -> [%e mk [%expr x]]) vx.d]
          else mk xv.e in
        [%expr let [%p (var n).p] = [%e get_field' n]
          and [%p xv.p] = [%e get_field' x] in [%e body] ]
      | Record_extension {exts;tag=tag,_ ;ptr= ptr, _ } ->
        Record_extension.merge (typename, exts) (get_field' tag) (get_field' ptr)
      | Simple (n, Array(Some (Lit i), ty )) when Aux.is_char ty ->
        [%expr Ctypes.string_from_ptr [%e start @@ get_field' n] [%e int.e i] ]
    | Simple (n, Array(Some (Const s), ty)) when Aux.is_char ty ->
      [%expr Ctypes.string_from_ptr [%e start @@ get_field' n] [%e (var s).e] ]
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


  let rec ty_of_int ty x =
    match ty with
    | Ty.Option t -> [%expr Some [%e ty_of_int t x]]
    | Name t -> [%expr [%e ident @@ qn t "of_int"] [%e x] ]
    | _ -> x
  let array_len x = [%expr Ctypes.CArray.length [%e x] ]

  let nullptr = function
    | Ty.Option _ -> [%expr None]
    | t -> coerce (ptr void) (Type.converter true t) [%expr Ctypes.null]
  (*   | ty -> not_implemented "Null array not implemented for %a@." Ty.pp ty *)



  let set types typ r field value =
    let name = varname % fst and ty = snd in
    let array_len (_, ty as _index) = ty_of_int ty (array_len value.e) in
    let optzero f = if Aux.is_option (ty f) then [%expr None] else [%expr 0] in
    let setf f x = setf r f x in
    match field with
    | Ty.Simple(f, (Ptr Name t | Const Ptr Name t)) when Aux.is_record types t ->
      setf (varname f) (addr @@ value.e)
    | Ty.Simple(f, (Option (Const Ptr Name t| Ptr Name t)
                   | Const Option(Const Ptr Name t |Ptr Name t))) when
        Aux.is_record types t ->
      setf (varname f) [%expr may Ctypes.addr [%e value.e]]
    | Ty.Simple(f, Array(Some Path _, _)) ->
      setf (varname f) (start value.e)
    | Ty.Simple (f,_ty) ->
      setf (varname f) value.e
    | Ty.Array_f { index; array } as t when Aux.is_option_f t ->
      [%expr match [%e value.e] with
        | None ->
          [%e setf (name index) (optzero index)];
          [%e setf  (name array) (nullptr @@ ty array)]
        | Some [%p value.p] ->
          [%e setf (name index) (array_len index) ];
          [%e setf (name array) (wrap_opt (ty array) @@ start value.e)]
      ]
    | Ty.Array_f { index; array } ->
      [%expr
        [%e setf (name index) (array_len index)];
        [%e setf (name array) (start value.e) ]
      ]
    | Ty.Record_extension { exts; _ } ->
      [%expr
        let type__gen, next__gen =
          [%e Record_extension.split (typ,exts) value.e ] in
        [%e setf "s_type" [%expr type__gen] ];
        [%e setf "next" [%expr next__gen] ]
      ]

  let rec printer types t =
    let printer = printer types in
    let pp module' = ident @@ (qn module') "pp" in
    let abstract = [%expr pp_abstract] in
    match t with
    | Ty.Name t ->
      begin match B.find_type t types with
        | exception Not_found -> pp t
        | Some (Ty.FunPtr _ |Union _ ) -> abstract
        | Some Ty.Bitfields _ -> pp (Bitset.set_name t)
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

  let seq  = listr (fun x y -> [%expr [%e x]; [%e y]])

  let rec sseq sep map = function
    | [] -> [%expr () ]
    | [a] -> map a
    | a :: q -> [%expr [%e map a]; [%e sep]; [%e sseq sep map q] ]

  let pp types fields =
    let u = unique "x" in
    let with_pf x = [%expr let pf ppf = Printer.fprintf ppf in [%e x]  ] in
    let pp_f (name,ty) =
        let fmt = string @@ varname name ^ "=%a" in
        [%expr pf ppf [%e fmt] [%e printer types ty]
            ([%e (var name).e]  [%e u.e]) ] in
    let def x =
      item
        [%stri let pp ppf = fun [%p u.p] ->
            let pf ppf = Printer.fprintf ppf in
            pf ppf "@[{@ "; [%e with_pf x] ]
        (val' ~:"pp"[%type: Format.formatter -> t -> unit])
    in
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
    let sep = [%expr pf ppf ";@ "] in
    def @@ [%expr [%e sseq sep pp_field fields]; pf ppf "@ }@]"]

  let keep_alive exprs owner body =
    let values = Exp.array @@ List.map (fun e -> [%expr Obj.repr [%e e]]) exprs in
    [%expr let () =
             Gc.finalise ( fun _ -> let _kept_alive = [%e values] in () ) [%e owner]
      in [%e body]
    ]

  let def_fields (type a) (typename, kind: _ * a kind) types (fields: a list) =
    let seal = hidden [%stri let () = Ctypes.seal t] in
    match kind with
    | Union -> module' inner (imap sfield fields) ^:: seal
    | Record ->
      let lens f = getter typename types fields (~:(repr_name f), f) in
      let exts= match Aux.record_extension fields with
        | Some exts -> Record_extension.def (typename,exts)
        | None -> nil in
      exts
      @*  module' inner
        (List.fold_right (fun x l -> field x @* l) fields nil)
      ^:: seal @* (imap lens fields) @* pp types fields ^:: nil

  let kind_cstr (type a) (kind: a kind) typ = match kind with
    | Union -> [%type: [%t typ] Ctypes.union]
    | Record -> [%type: [%t typ] Ctypes.structure]

  let ke (type a) (kind:a kind) = match kind with
    | Union -> [%expr union]
    | Record -> [%expr structure]

  let def types (_,kind as tk) name fields =
    hidden [%stri type mark ]
    @* item [%stri type t = [%t kind_cstr kind [%type:mark] ] ] [%sigi: type t]
    ^:: item
      [%stri let t: t Ctypes.typ = [%e ke kind] [%e string @@ typestr name] ]
      (val' ~:"t" [%type: t Ctypes.typ])
    ^:: def_fields tk types fields

  let keep_field_alive vars acc = function
    (*  | Ty.Simple(_,Array _ ) -> acc *)
    | Ty.Simple(f, _ ) -> ex (M.find @@ varname f) vars :: acc
    | _ -> acc

  let array =
    reset_uid ();
    let input = unique "input" and array = unique "array" in
    let keep = keep_alive [array.e] input.e array.e in
    item
      [%stri let array = fun [%p input.p] ->
          let [%p array.p] = Ctypes.CArray.of_list t [%e input.e] in
          [%e keep]
      ]
      (val' ~:"array" [%type: t Ctypes.CArray.t ] )

     let construct types tyname fields =
    let fn, m = mkfun fields in
    let res = unique "res" in
    let set field = set types tyname res.e field (M.find (repr_name field) m) in
    let keep_alive =
      keep_alive (List.fold_left (keep_field_alive m) [] fields) res.e in
    let body =
      [%expr let [%p res.p] = Ctypes.make t in
        [%e keep_alive @@ seq set fields res.e ]
      ] in
    item [%stri let make = [%e fn body]]
      (val' ~:"make" @@ Type.fn ~with_label:true tyname fields [%type: t])

  let make (type a) types (kind: a kind) (name, fields: _ * a list) =
    let records = match kind with
      | Union -> imap union fields
      | Record -> construct types name fields ^:: nil in
    module' name
      (def types (name,kind) name fields @* array ^:: records)
    ^:: item
      [%stri let [%p pat var name] = [%e ident @@ qn name "t"]]
       (val' name @@ typ ~par:name ~:"t")
    ^::  extern_type name
    ^:: nil

end

module Funptr = struct

  let mkty args ret =
    let ret  = Type.converter true ret in
    listr (fun l r -> [%expr[%e l] @-> [%e r] ]) (Type.converter true)
      args
      [%expr returning [%e ret]]

  let expand = function
    | [] -> [Ty.Name (L.simple ["void"])]
    | l -> l

  let make (tyname, (fn:Ty.fn)) =
    let ty = pty tyname and tyo = pty L.(tyname//"opt")in
    match List.map snd @@ Ty.flatten_fn_fields fn.args with
    | [] -> item
              [[%stri let [%p ty] = ptr void]]
              [val' tyname [%type: void Ctypes.ptr]]
    | args ->
      let t = Type.fn fn.name (Aux.to_fields fn.args) (Type.mk fn.return) in
      item
        [[%stri let [%p ty], [%p tyo] =
                 let ty = [%e mkty args fn.return] in
                 Foreign.funptr ty, Foreign.funptr_opt ty
        ]]
        [ val' tyname t;
          val' L.(tyname//"opt") [%type: [%t t] option] ]

end


module Fn = struct
  open Funptr

  let arg_types (fn:Ty.fn) =
    expand @@ List.map snd @@ Ty.flatten_fn_fields fn.args

  let foreign fn =
    let args = arg_types fn in
    [%expr foreign [%e string fn.original_name] [%e mkty args fn.return]]

  let make_simple (fn:Ty.fn) =
    [%stri let [%p (var fn.name).p] = [%e foreign fn] ]

  let apply_gen get name vars args =
    let get f = Asttypes.Nolabel, get vars f in
    let add_arg l = function
      | Ty.Array_f { array ; index } ->
         (get array) :: (get index) :: l
      | Simple field -> get field :: l
      | Record_extension _  -> assert false in
    let args = List.rev @@ List.fold_left add_arg [] args in
    Exp.apply name args

  let apply = apply_gen (fun vars (f,_ty) -> ex (M.find @@ varname f) vars)


  let get_r types vars (f,ty) =
    regularize types ty @@ ex (M.find @@ varname f) vars
  let apply_regular types = apply_gen (get_r types)

  let mkfn_simple fields =
    let build (f,vars) (name,_ty) =
      let u = unique (varname name) in
      (fun body -> f [%expr fun [%p u.p] -> [%e body] ]),
      M.add (varname name) u vars in
    List.fold_left build ((fun x -> x), M.empty) fields

  let make_regular types fn =
    let args = Aux.to_fields fn.Ty.args in
    let f = unique "f" in
    let def body =
      [%stri let [%p pat var fn.name] =
               let [%p f.p] = [%e foreign fn] in
               [%e body] ] in
    def begin
      let fe, vars = mkfn_simple @@ Ty.flatten_fields args in
      fe @@ apply_regular types f.e vars args
    end

  let make_labelled m fn =
    let args = Aux.to_fields fn.Ty.args in
    let k, vars = mkfun args in
    [%stri let make =
             [%e k @@ apply (ident @@ qn m @@ varname fn.name) vars args]
    ]


  let rec ptr_to_name ?(ellide=true) = function
    | Ty.Option t -> ptr_to_name ~ellide t
    | Ty.Name t -> Some(ident (typename t), t )
    | Ty.Ptr p | Array(_,p) ->
      begin match ptr_to_name ~ellide:false p with
        | Some(p,elt) -> Some((if ellide then p else ptr p),elt)
        | None -> None
      end
    | _ -> None

  let nullptr_typ p = [%expr nullptr [%e p]]
  let allocate_n ty n = [%expr Ctypes.allocate_n [%e ty] [%e n]]

  (* Allocate composite fields *)
  let allocate_field types fields vars f body  =
    let get f = M.find (varname f) vars in
    match f.Ty.field with
    | Simple(f, Array(Some(Path p), Name elt)) ->
      let array = get f in
      let size =  get @@ index_name f in
      let pty = Aux.type_path types fields p in
      let n = Structured.get_path (ex get) pty in
      [%expr let [%p size.p] = [%e n ] in
             let [%p array.p ] = Ctypes.allocate_n [%e (var elt).e ] [%e size.e ] in
               [%e body]
      ]
    | Simple(f, Name t) when Aux.is_record types t ->
      let f = get f in
      [%expr let [%p f.p ] =
               Ctypes.make [%e ex var t] in [%e body] ]
    | Simple (f,t) ->
      begin let f = get f in
        match ptr_to_name t with
        | None -> body
        | Some (p,_) ->
          let alloc = wrap_opt t @@ allocate_n p [%expr 1] in
            [%expr let [%p f.p] = [%e alloc] in [%e body] ]
      end
    | Array_f { array=a, Option _; index=i, Ptr Option Name t } ->
      let a = get a and i = get i in
      [%expr let [%p i.p] = Ctypes.allocate [%e ex var L.(t//"opt")] None in
        let [%p a.p] = None in [%e body]
      ]
    | Array_f { array=a, elt; index=i, size } ->
      let a = get a and i = get i in
      begin match ptr_to_name elt, ptr_to_name size with
        | None, _ | _, None -> body
        | Some (e,_), Some(s,_) ->
          let alloc_size = wrap_opt size @@ allocate_n s [%expr 1]
          and alloc_elt = nullptr_typ e in
          [%expr let [%p a.p] = [%e alloc_elt] and [%p i.p] = [%e alloc_size] in
            body
          ]
      end
    | _ -> not_implemented "Native function for field type %a" Ty.pp_fn_field f

  (* Array output parameter needs to be allocated in two times:
     first the index parameter is allocated,
     then the function is applied and fills the actual value of the
     index parameter, which enable us to allocate the output array *)
  let secondary_allocate_field vars f body = match f.Ty.field with
    | Array_f { array = a, tya; index = i, it  } ->
      let a = M.find (varname a) vars and i = M.find (varname i) vars in
      let size = Structured.int_of_ty i.e it in
      begin match ptr_to_name tya with
        | Some (elt, _ ) ->
          let alloc = allocate_n elt size in
          [%expr let [%p a.p] = [%e wrap_opt tya @@ alloc] in [%e body] ]
        | None -> assert false end
    | _ -> body

  let extract_opt input output nullptr map body =
    let scrutinee = unique "scrutinee" in
    [%expr let [%p output.p] = match [%e input] with
        | None -> None, [%e nullptr]
        | Some [%p scrutinee.p] -> [%e map scrutinee.e] in  [%e body]
    ]

  let len' x = [%expr Ctypes.CArray.length [%e x] ]
  let len ty x = Structured.ty_of_int ty @@ len' x

  let start = Structured.start
  let extract_array input (ty,index) array body =
    [%expr
      let [%p index] = [%e len ty input] in
      let [%p array] = [%e start input] in
      [%e body]
    ]

  let (<*>) x y =
    { p = [%pat? [%p x.p], [%p y.p] ]; e = [%expr [%e x.e], [%e y.e] ] }

  let nullptr = Structured.nullptr

  let input_expand vars f body = match f with
    | Ty.Array_f { array= (a, tya ) ; index = (i, ty )  } as f ->
      let a = M.find (varname a) vars and i = M.find (varname i) vars in
        if Aux.is_option_f f then
          let extract_array input =
            [%expr [%e len ty input], [%e start input]] in
          extract_opt a.e ( i <*> a )
            (nullptr tya) extract_array body
        else
          extract_array a.e (ty,i.p) a.p body
    | Simple(f, Array(Some Path _, ty)) ->
      let f = M.find (varname f) vars in
      if Aux.is_option ty then
        [%expr let [%p f.p] = match [%e f.e] with
            | Some [%p f.p] -> Some [%e start f.e]
            | None -> None in [%e body]
        ]
      else
        [%expr let [%p f.p] =[%e start f.e] in [%e body] ]
    | _ -> body

  let tuple l = Exp.tuple l

  let ty_of_int = Structured.ty_of_int
  let int_of_ty = Structured.int_of_ty
  let from_ptr x y = [%expr Ctypes.CArray.from_ptr [%e x] [%e y] ]
  let to_output types vars f =
    let get x = ex (M.find @@ varname x) vars in
    match f.Ty.field with
    | Ty.Array_f { array = (n, Ty.Option _) ; index = i , it } ->
      from_ptr [%expr unwrap [%e get n]] (int_of_ty (get i) it)
      | Ty.Array_f { array = (n,_) ; _ } -> get n
      | Simple(f, Array(Some(Path _), Name _ )) ->
        from_ptr (get f)
          (ex (M.find @@ varname @@ index_name f) vars)
      | Simple(n, Name t)  when Aux.is_record types t ->  get n
      | Simple(n, _) -> [%expr Ctypes.(!@) [%e get n] ]
      | Record_extension _ ->
        not_implemented "Record extension used as a function argument"

  let join ty res outputs =
    let n = List.length outputs in
    if Aux.is_result ty then
      let u = unique "ok" in
      let outputs = if outputs = [] then [[%expr ()]] else outputs in
      [%expr match [%e res] with
        | Error _ as e -> e
        | Ok [%p u.p] -> Ok [%e tuple @@ u.e :: outputs]
      ]
    else if  n = 0 then
      res
    else if Aux.is_void ty then
      tuple outputs
    else
      tuple @@ res :: outputs

  let look_out vars output = List.fold_left ( fun (l,vars) f ->
      match f.Ty.field with
      | Ty.Array_f { array = a, _ ; index = i, _ } ->
        let u = unique (varname a) and v = unique (varname i) in
        u.e :: l, vars |> M.add (varname i) u |> M.add (varname a) v
      | Simple(n, Array _ ) ->
        let u = unique (varname n) and v = unique "size" in
        u.e :: l, vars |> M.add (varname n) v |> M.add (varname @@ index_name n) u
      | f ->
        let u = unique (repr_name f) in
        u.e :: l, vars |> M.add (repr_name f) u
        ) ([], vars) output

  let make_native types (fn:Ty.fn)=
    reset_uid ();
    let rargs = regularize_fields types fn.args in
    let fold f l body = List.fold_right ((@@) f ) l body in
    let input, output =
      List.partition (fun r -> r.Ty.dir = In || r.dir = In_Out) rargs in
    let apply_twice = List.exists
        (function { Ty.field = Array_f _ ; _ } -> true | _ -> false) output in
    let tyret = fn.return in
    let input' = Aux.to_fields input in
    let all = Aux.to_fields fn.args in
    let fun', vars = mkfun input' in
    let _, vars = look_out vars output in
    (fun x -> [%stri let [%p pat var fn.name] = [%e x] ]) @@
    fun' @@
    fold (input_expand vars) input' @@
    fold (allocate_field types input' vars) output @@
    let apply = apply_regular types (ex var fn.name) vars all in
    let res = unique "res" in
    let result =
      let outs = List.map (to_output types vars) output in
      [%expr let [%p res.p] = [%e apply] in [%e join tyret res.e outs] ] in
    let secondary = fold (secondary_allocate_field vars) output in
    if not apply_twice then
      result
    else if Aux.is_result fn.return then
      [%expr match [%e apply] with
        | Error _ as e -> e
        | Ok _ -> [%e secondary result]
      ]
    else
      [%expr [%e apply]; [%e secondary result] ]

  let make types = function
    | B.Regular -> make_regular types
    | Native -> make_native types
    | Raw -> make_simple
end

let packed m = Exp.pack H.Mod.(ident @@ nlid @@ modname m)
let alias builtins (name,origin) =
  if not @@ B.Name_set.mem name builtins then
    let sign =
      let constraint' = H.Type.mk ~manifest:(typ ~par:(~:"X") ~:"t") (nloc "x") in
      H.Mty.(with_ (ident @@ nlid "alias") [P.Pwith_typesubst constraint'] ) in
    let t = typ ~par:(~:"Alias") ~:"t" in
    ( module_gen name @@
      item
        H.Mod.(apply (ident (nlid "Alias"))
                 (ident @@ nlid @@ modname origin))
        sign
    )
    ^:: item
      [%stri let [%p pat var name] = [%e ident @@ qn name "ctype"] ]
      (val' name [%type: [%t t] Ctypes.typ])
    ^:: item
      [%stri let [%p pat var L.(name//"opt")] = integer_opt [%e packed name] ]
       (val' L.(name//"opt") [%type: [%t t] option Ctypes.typ])
    ^:: nil
else
    nil

let float_const f = Exp.constant (H.Const.float @@ string_of_float f)
module Const = struct
  let make (name,const) =
    let rec expr =
      function
      | B.Arith.Float f -> float_const f
      | Int n ->  int.e n
      | UInt64 n ->
        [%expr Unsigned.ULLong.of_string [%e string @@ Unsigned.ULLong.to_string n]]
      | UInt n ->
        [%expr Unsigned.UInt.of_string [%e string @@ Unsigned.UInt.to_string n] ]
      | Complement num_expr -> [%expr lnot [%e expr num_expr] ]
      | Minus (a,b) -> [%expr [%e expr a] - [%e expr b] ] in
    [%stri let [%p pat var name] = [%e expr const] ]
end
