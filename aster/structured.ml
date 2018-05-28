module Aliases= struct
  module L = Info.Linguistic
  module B = Lib
  module Ty = B.Ty
  module T = B.T
  module H = Ast_helper
  module Exp = H.Exp
  module C = Common
  module M = Info.Common.StringMap
end
open Aliases
open Item
open Utils

let unique, reset_uid = C.id_maker ()

let of_int = L.simple ["of"; "int"]
let to_int = L.simple ["to"; "int"]

let ($) f x = [%expr [%e f] [%e x] ]

let tn = ident @@ lid  @@ "ctype"
let tn0 = ident @@ lid  @@ "view"

let (#?) = B.(#?)
let normalize_ty_name ctx name = match ctx#?(name) with
  | Some Bitfields _ -> Bitset.set_name name
  | _ -> name

let rec ty_of_int ctx ty x =
  match ty with
  | Ty.Option t -> [%expr Some [%e ty_of_int ctx t x]]
  | Name t ->
    let f = ident @@
      Inspect.prefix varpath ~name:of_int ctx
      @@ normalize_ty_name ctx t in
    f $ x <?> "ty_of_int"
  | _ -> x <?> "Ty_of_int failure?"

let rec int_of_ty ctx var = function
  | Ty.Ptr t -> int_of_ty ctx [%expr Ctypes.(!@) [%e var]] t
  | Option t -> int_of_ty ctx [%expr Vk__helpers.unwrap [%e var]] t
  | Name t when L.to_path t = ["uint32";"t"] -> var
  | Name t ->
    let f = ident
      @@ Inspect.prefix varpath ~name:to_int ctx
      @@ normalize_ty_name ctx t in
    f $ var <?> "Structured int_of_ty"
  | _ -> raise @@ Invalid_argument "Invalid Structured.int_of_ty ty"


let mkfun fields =
  let label f name =
    if Inspect.is_option_f f then
      Asttypes.Optional name
    else
      Asttypes.Labelled name in
  let add_arg (k,m) =
    let def  ?opt f p body = k @@ Exp.fun_
        (label f @@ varname @@ C.repr_name f) opt p body in
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
      def f u.p, vars [varname n, u; varname @@ C.index_name n, v]
    | Ty.Simple(n,_)  as f ->
      let u = unique (varname n) in
      def f u.p, vars [varname n, u] in
  let terminator =
    if List.exists Inspect.is_option_f fields || List.length fields = 0
    then fun body -> [%expr fun () -> [%e body]]
    else fun x -> x in
  let f, m = List.fold_left add_arg ((fun x->x), M.empty) fields in
  f % terminator, m


type 'field kind =
  | Union: Ty.simple_field kind
  | Record: Ty.field kind

let sfield types (name,t) =
  (* Note: we could try to simplify further field names,
     but they happen to be quite short in practice *)
  let str = varname name and
  conv = Type.converter types false t in
  let ty = Type.mk ~raw_type:true ~decay_array:Dyn_array types t in
  item
    [%stri let [%p pat var name] = Ctypes.field [%e tn] [%e string str] [%e conv] ]
    (val' name [%type: ([%t ty] , t) Ctypes.field])

let addr_i = item
    [%stri let addr = Ctypes.addr ]
    (val' ~:"addr" [%type: t -> t Ctypes.ptr ])


let unsafe_make_i = item
    [%stri let unsafe_make () = Ctypes.make [%e tn] ]
    (val' ~:"unsafe_make" [%type: unit -> t ])


let tymod n = L.simple [Fmt.strf "Vk__Types__%a" L.pp_module n]

let pretyp ctx n x =
  let q = tymod n in
  ident @@ qualify [q] x

let unsafe_make ctx n =
  [%expr [%e pretyp ctx n "unsafe_make"] () ]

let field types =
  let sfield = sfield types in
  function
  | Ty.Simple f -> sfield f ^:: nil
  | Array_f r -> sfield r.index ^:: sfield r.array ^:: nil
  | Record_extension {tag; ptr; _ } ->
    sfield tag ^:: sfield ptr ^:: nil

let (#.) r x = [%expr Ctypes.getf [%e r] [%e x]]
let (#%) r get = [%expr [%e get] [%e r] ]
let fname x = Exp.ident (nloc @@ lid "Fields" / x)
let get_field r f = r #. [%expr [%e fname f]]
let get_field' r = get_field r % varname
let mk_array n x = [%expr Ctypes.CArray.from_ptr [%e x] [%e n]]

let rec get_path ctx f = function
  | [] -> raise @@ Invalid_argument "Printers.pp_path: empty path"
  | [None,_, a] -> f a
  | (Some (direct, ty),_, a) :: q ->
    (if direct then get_path ctx f q else
       [%expr Ctypes.(!@) [%e get_path ctx f q]])
    #% (pretyp ctx ty (varname a) )
  | (None, _, _) :: _ -> raise @@
    Invalid_argument "Printers.pp_path: partially type type path"



let imay f x = [%expr Vk__helpers.may [%e f] [%e x] ]

let rec oint_of_ty ctx var = function
  | Ty.Ptr t -> oint_of_ty ctx [%expr Ctypes.(!@) [%e var]] t
  | Option t ->
    let v = unique "i" in
    imay [%expr fun [%p v.p] ->[%e oint_of_ty ctx v.e t]] var
  | ty -> int_of_ty ctx var ty

let int_of_path ?(conv=true) ctx get_field pty =
  let p = get_path ctx get_field pty in
  if conv then
    oint_of_ty ctx p (Inspect.last_type pty)
  else p

let tuple =
  {e= (function
       | [x] -> x
       | l -> H.Exp.tuple l);
   p=(function
       | [p] -> p
       | l -> H.Pat.tuple l
     )
  }

let array_index ?(conv=true) ctx get_field fields = function
  | Ty.Lit n -> int.e n
  | Path p -> int_of_path ctx ~conv get_field
                (Inspect.type_path ctx fields p)
  | Math_expr m ->
    let vars = T.vars [] m in
    let v = tuple.e @@
      List.map
        (fun p -> let pty = Inspect.type_path ctx fields [p] in
          int_of_path ~conv ctx get_field pty)
        vars in
    let n = tuple.p @@ (List.map (fun x -> pat var x) vars) in
    [%expr let [%p n] = [%e v] in [%e Math.expr m] ]
  | _ -> assert false


let start x = [%expr Ctypes.CArray.start [%e x]]

let inner = ~:"Fields"

let setf r f value =
  [%expr Ctypes.setf [%e r] [%e ident(qn inner @@ f)] [%e value] ]

let union types (n,ty) =
  reset_uid ();
  let u = unique "x" and r = unique "res" in
  item [%stri let [%p pat var n] = fun [%p u.p] ->
      let [%p r.p] = Ctypes.make [%e tn] in
      [%e setf r.e (varname n) u.e ]; [%e r.e]
  ]
    (val' n [%type: [%t Type.mk types ty] -> t ])

let type_field types typename = function
  | Ty.Simple(_,ty) -> Type.mk types ty
  | Array_f { array = _, ty; index = _, ity }
    when Inspect.is_option ity && not (Inspect.is_option ty) ->
    [%type: [%t Type.mk types ty] option ]
  | Array_f { array = _, ty; _ } -> Type.mk types  ty
  | Record_extension _ -> typ L.(typename//"ext")

let const x = varpath ~par:[L.simple["Vk__Const"]] x

let getter typename types fields field =
  reset_uid ();
  let u = unique "record" in
  let get_field' = get_field' u.e and get_field = get_field u.e in
  let type' = type_field types typename field in
  let main x = [C.repr_name field, type', x] in
  let def = fold_map
      (fun (out, ty, x) -> item
          [%str let [%p pat var out] = fun [%p u.p] -> [%e x] ]
          [val' out [%type: t -> [%t ty]] ]
      ) in
  def begin match field with
    | Ty.Array_f {index=i,ty; array = a, tya} as f when Inspect.is_option_f f ->
      let index =
        int_of_ty types (ex ident' "n") (C.unwrap_opt_ty ty) in
      let count = [i , Type.mk types ty, get_field' i] in
      count
      @ main
        [%expr match [%e get_field' i], [%e get_field' a] with
          | [%p if Inspect.is_option ty then [%pat? Some n] else [%pat? n]],
            [%p if Inspect.is_option tya then [%pat? Some a] else [%pat? a]]
            ->
            Some [%e mk_array index [%expr a] ]
          | _ -> None
        ]
    | Array_f {array= x, tya; index = n, ty } ->
      let count = [n , Type.mk types ty, get_field' n ] in
      let mk = mk_array (int_of_ty types (ex var n) ty) in
      let xv = var x in
      let body = if Inspect.is_option tya then
          imay [%expr fun x -> [%e mk [%expr x]] ] xv.e
        else mk xv.e in
      count @ main
        [%expr let [%p (var n).p] = [%e get_field' n]
          and [%p xv.p] = [%e get_field' x] in [%e body] ]
    | Record_extension {exts;tag=tag,_ ;ptr= ptr, _ } ->
      main @@
      Record_extension.merge (typename, exts) (get_field' tag)
        (get_field' ptr)
    | Simple (n, Array(Some (Lit i), ty )) when Inspect.is_char ty ->
      main [%expr Ctypes.string_from_ptr [%e start @@ get_field' n]
          [%e int.e i] ]
    | Simple (n, Array(Some (Const s), ty)) when Inspect.is_char ty ->
      main[%expr Ctypes.string_from_ptr [%e start @@ get_field' n]
          [%e ident (const s) ] ]
    | Simple(n, (Option Array(Some (Path _ | Math_expr _ as p), inty)
                | Array(Some (Path _ | Math_expr _ as p),inty) as t)) ->
      let index = array_index types get_field' fields p in
      let a =
        if Inspect.is_char inty then
          [%expr Ctypes.string_from_ptr (Ctypes.CArray.start a) i]
        else
          mk_array [%expr i] [%expr a] in
      let body = let aopt = Inspect.is_option t
        and opt = match p with
          | Path p ->
            Inspect.final_option types fields p
          | _ -> false in
        if opt && aopt then
          [%expr Vk__helpers.maybe
              [%e imay [%expr fun i a -> [%e a]] [%expr i] ]
              a]
        else if opt then
          imay [%expr fun i -> [%e a]] [%expr i]
        else if aopt then
          imay  [%expr fun a -> [%e a]] [%expr a]
        else a in
      main [%expr let a = [%e get_field' n]
        and i = [%e index] in [%e body] ]
    | Simple(name,_) -> main @@ get_field (varname name)
  end





let array_len x = [%expr Ctypes.CArray.length [%e x] ]

let nullptr types = function
  | Ty.Option _ -> [%expr None]
  | t ->
    C.coerce C.(ptr void) (Type.converter types true t)
      [%expr Ctypes.null]

let convert_string n s =
  [%expr Vk__helpers.convert_string [%e n] [%e s] ]


let set types typ r field value =
  let name = varname % fst and ty = snd in
  let array_len (_, ty as _index) =
    ty_of_int types ty (array_len value.e) in
  let optzero f = if Inspect.is_option (ty f) then [%expr None] else [%expr 0] in
  let setf f x = setf r f x in
  match field with
  | Ty.Simple(f, (Ptr Name t | Const Ptr Name t)) when Inspect.is_record types t ->
    setf (varname f) (C.addrf types t $  value.e)
  | Ty.Simple(f, (Option (Const Ptr Name t | Ptr Name t)
                 | Const Option(Const Ptr Name t | Ptr Name t))) when
      Inspect.is_record types t ->
    setf (varname f) (imay (C.addrf types t) value.e)
  | Ty.Simple(f, Array(Some (Lit n), t)) when Inspect.is_char t ->
    setf (varname f)
      (convert_string (int.e n) value.e)
  | Ty.Simple(f, Array(Some (Const n), t)) when Inspect.is_char t ->
    setf (varname f) (convert_string (ident @@ const n) value.e)
  | Ty.Simple(f, Array(Some (Path _| Math_expr _), _)) ->
    setf (varname f) (start value.e)
  | Ty.Simple(f, Option Array(Some (Path _|Math_expr _), _)) ->
    setf (varname f) (imay [%expr Ctypes.CArray.start] value.e)
  | Ty.Simple (f,_ty) ->
    setf (varname f) value.e
  | Ty.Array_f { index; array } as t when Inspect.is_option_f t ->
    [%expr match [%e value.e] with
      | None ->
        [%e setf (name index) (optzero index)];
        [%e setf  (name array) (nullptr types @@ ty array)]
      | Some [%p value.p] ->
        [%e setf (name index) (array_len index) ];
        [%e setf (name array) (C.wrap_opt (ty array) @@ start value.e)]
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
  let pp module' =
    ident @@
    Inspect.prefix varpath types ~name:(~:"pp") module' in
  let abstract = [%expr Vk__helpers.Pp.abstract] in
  match t with
  | Ty.Name t ->
    begin match B.find_type t types with
      | exception Not_found -> pp t
      | Some (Ty.FunPtr _ |Union _ ) -> abstract
      | Some Ty.Bitfields _ -> pp (Bitset.set_name t)
      | _ -> pp t end
  | Array(_, Name t) when L.to_path t = ["char"] ->
    [%expr Vk__helpers.Pp.string]
  | Array(_,t) -> [%expr Vk__helpers.Pp.array [%e printer t]]
  | Option t -> [%expr Vk__helpers.Pp.opt [%e printer t]]
  | Const t -> printer t
  | Ptr t when Inspect.is_void t -> [%expr Vk__helpers.Pp.addr]
  | Ptr t -> [%expr Vk__helpers.Pp.ptr [%e printer t]]
  | String -> [%expr Vk__helpers.Pp.string]
  | ty -> Fmt.epr "Not implemented: %a@." Ty.pp ty;
    raise @@ Invalid_argument "Printer not implemented"

let seq  = C.listr (fun x y -> [%expr [%e x]; [%e y]])

let rec sseq sep map = function
  | [] -> [%expr () ]
  | [a] -> map a
  | a :: q -> [%expr [%e map a]; [%e sep]; [%e sseq sep map q] ]

let pp types fields =
  let u = unique "x" in
  let with_pf x = [%expr let pf ppf = Format.fprintf ppf in [%e x]  ] in
  let pp_f (name,ty) =
    let fmt = string @@ varname name ^ "=%a" in
    [%expr pf ppf [%e fmt] [%e printer types ty]
        ([%e (var name).e]  [%e u.e]) ] in
  let def x =
    item
      [%stri let pp ppf = fun [%p u.p] ->
          let pf ppf = Format.fprintf ppf in
          pf ppf "@[{@ "; [%e with_pf x] ]
      (val' ~:"pp"[%type: Format.formatter -> t -> unit])
  in
  let pp_field (field:Ty.field) = match field with
    | Record_extension _ -> [%expr pf ppf "ext=⟨unsupported⟩"]
    | Array_f { array=name, (Ty.Ptr ty| Const Ptr ty | Array(_,ty));
                index=_,tyi  } ->
      let opt x = if Inspect.is_option tyi then Ty.Option x else x in
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
  [%expr
    let () = try
        Gc.finalise_last
          ( fun () -> let _kept_alive = [%e values] in () ) [%e owner]
      with Invalid_argument _ -> () (* FIXME *)
    in [%e body]
  ]


let def_fields (type a) (typename, kind: _ * a kind) types (fields: a list) =
  let seal = hidden [%stri let () = Ctypes.seal [%e tn] ] in
  match kind with
  | Union -> module' inner (structure @@ imap (sfield types) fields) ^:: seal
  | Record ->
    let lens f = getter typename types fields f in
    let exts= match Inspect.record_extension fields with
      | Some exts -> Record_extension.def (typename,exts)
      | None -> nil in
    exts
    @*  module' inner
      (structure @@ List.fold_right (fun x l -> field types x @* l) fields nil)
    ^:: seal @* (fold_map lens fields) @* pp types fields ^:: nil

let kind_cstr (type a) (kind: a kind) typ = match kind with
  | Union -> [%type: [%t typ] Ctypes.union]
  | Record -> [%type: [%t typ] Ctypes.structure]

let ke (type a) (kind:a kind) = match kind with
  | Union -> [%expr Ctypes.union]
  | Record -> [%expr Ctypes.structure]

let def types (_,kind as tk) name fields =
  hidden [%stri type mark ]
  @* item [%stri type t = [%t kind_cstr kind [%type:mark] ] ] [%sigi: type t]
  ^:: item
    [%stri let view: t Ctypes.typ =
             [%e ke kind] [%e string @@ typestr name] ]
    (val' ~:"view" [%type: t Ctypes.typ])
 ^:: item
    [%stri let ctype = view ]
    (val' ~:"ctype" [%type: t Ctypes.typ])
  ^:: def_fields tk types fields
  @* addr_i
  ^:: unsafe_make_i ^:: nil

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
        let [%p array.p] = Ctypes.CArray.of_list [%e tn] [%e input.e]
        in [%e keep]
    ]
    (val' ~:"array" [%type: t list -> t Ctypes.CArray.t ] )

let construct types tyname fields =
  let fn, m = mkfun fields in
  let res = unique "res" in
  let set field = set types tyname res.e field
      (M.find (varname @@ C.repr_name field) m) in
  let keep_alive =
    keep_alive (List.fold_left (keep_field_alive m) [] fields) res.e in
  let body =
    [%expr let [%p res.p] = Ctypes.make [%e tn] in
      [%e keep_alive @@ seq set fields res.e ]
    ] in
  item [%stri let make = [%e fn body]]
    (val' ~:"make" @@
     Type.fn types ~regular_struct:true ~with_label:true tyname fields [%type: t])

let raw = L.(~:"Raw")

let make (type a) types (kind: a kind) (name, fields: _ * a list) =
  let records = match kind with
    | Union -> imap (union types) fields
    | Record -> construct types name fields ^:: nil in
  def types (name,kind) name fields
  @* array ^:: records
