module Aliases= struct
  module L = Name_study
  module B = Lib_builder
  module Ty = Lib_builder.Ty
  module T = Lib_builder.T
  module H = Ast_helper
  module Exp = H.Exp
  module Inspect = Ast__inspect
  module C = Ast__common
  module M = Enkindler_common.StringMap
end
open Aliases
open Ast__item
open Ast__utils

let unique, reset_uid = C.id_maker ()

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
  let str = varname name and conv = Ast__type.converter false t in
  let ty = Ast__type.mk ~raw_type:true ~decay_array:Dyn_array types t in
  item
    [%stri let [%p pat var name] = field t [%e string str] [%e conv] ]
    (val' name [%type: ([%t ty] , t) Ctypes.field])

let addr_i = item
    [%stri let addr = Ctypes.addr ]
    (val' ~:"addr" [%type: t -> t Ctypes.ptr ])


let unsafe_make_i = item
    [%stri let unsafe_make () = Ctypes.make t ]
    (val' ~:"unsafe_make" [%type: unit -> t ])

let unsafe_make n = [%expr [%e ident ( lid (modname n) /  "unsafe_make") ] () ]

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

let rec get_path f = function
  | [] -> raise @@ Invalid_argument "Printers.pp_path: empty path"
  | [None,_, a] -> f a
  | (Some (direct, ty),_, a) :: q ->
    (if direct then get_path f q else [%expr Ctypes.(!@) [%e get_path f q]])
    #% (ident (lid  (modname ty) / (varname a)))
  | (None, _, _) :: _ -> raise @@
    Invalid_argument "Printers.pp_path: partially type type path"

let rec int_of_ty var = function
  | Ty.Ptr t -> int_of_ty [%expr Ctypes.(!@) [%e var]] t
  | Option t -> int_of_ty [%expr unwrap [%e var]] t
  | Name t when L.to_path t = ["uint32";"t"] -> var
  | Name t -> [%expr [%e ident @@ qn t "to_int"][%e var] ]
  | _ -> raise @@ Invalid_argument "Invalid Printers.Fn.to_int ty"

let rec oint_of_ty var = function
  | Ty.Ptr t -> oint_of_ty [%expr Ctypes.(!@) [%e var]] t
  | Option t ->
    let v = unique "i" in
    [%expr may (fun [%p v.p] ->[%e oint_of_ty v.e t]) [%e var]]
  | Name t when L.to_path t = ["uint32";"t"] -> var
  | Name t -> [%expr [%e ident @@ qn (C.module_name t) "to_int"][%e var] ]
  | _ -> raise @@ Invalid_argument "Invalid Printers.Fn.to_int ty"

let int_of_path ?(conv=true) get_field pty =
  let p = get_path get_field pty in
  if conv then
    oint_of_ty p (Inspect.last_type pty)
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

let array_index ?(conv=true) get_field types fields = function
  | Ty.Lit n -> int.e n
  | Path p -> int_of_path ~conv get_field (Inspect.type_path types fields p)
  | Math_expr m ->
    let vars = T.vars [] m in
    let v = tuple.e @@
      List.map
        (fun p -> let pty = Inspect.type_path types fields [p] in
          int_of_path ~conv get_field pty)
        vars in
    let n = tuple.p @@ (List.map (fun x -> pat var x) vars) in
    [%expr let [%p n] = [%e v] in [%e Ast__math.expr m] ]
  | _ -> assert false


let start x = [%expr Ctypes.CArray.start [%e x]]

let inner = ~:"Fields"

let setf r f value =
  [%expr Ctypes.setf [%e r] [%e ident(qn inner @@ f)] [%e value] ]

let union types (n,ty) =
  reset_uid ();
  let u = unique "x" and r = unique "res" in
  item [%stri let [%p pat var n] = fun [%p u.p] ->
      let [%p r.p] = Ctypes.make t in
      [%e setf r.e (varname n) u.e ]; [%e r.e]
  ]
    (val' n [%type: [%t Ast__type.mk types ty] -> t ])

let type_field types typename = function
  | Ty.Simple(_,ty) -> Ast__type.mk types ty
  | Array_f { array = _, ty; index = _, ity }
    when Inspect.is_option ity && not (Inspect.is_option ty) ->
    [%type: [%t Ast__type.mk types ty] option ]
  | Array_f { array = _, ty; _ } -> Ast__type.mk types  ty
  | Record_extension _ -> typ L.(typename//"ext")

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
      let index = int_of_ty (ex ident' "n") (C.unwrap_opt_ty ty) in
      let count = [i , Ast__type.mk types ty, get_field' i] in
      count
      @ main
        [%expr match [%e get_field' i], [%e get_field' a] with
          | [%p if Inspect.is_option ty then [%pat? Option.Some n] else [%pat? n]],
            [%p if Inspect.is_option tya then [%pat? Option.Some a] else [%pat? a]]
            ->
            Option.Some [%e mk_array index [%expr a] ]
          | _ -> Option.None
        ]
    | Array_f {array= x, tya; index = n, ty } ->
      let count = [n , Ast__type.mk types ty, get_field' n ] in
      let mk = mk_array (int_of_ty (ex var n) ty) in
      let xv = var x in
      let body = if Inspect.is_option tya then
          [%expr may (fun x -> [%e mk [%expr x]]) vx.d]
        else mk xv.e in
      count @ main
        [%expr let [%p (var n).p] = [%e get_field' n]
          and [%p xv.p] = [%e get_field' x] in [%e body] ]
    | Record_extension {exts;tag=tag,_ ;ptr= ptr, _ } ->
      main @@
      Ast__record_extension.merge (typename, exts) (get_field' tag)
        (get_field' ptr)
    | Simple (n, Array(Some (Lit i), ty )) when Inspect.is_char ty ->
      main [%expr Ctypes.string_from_ptr [%e start @@ get_field' n]
          [%e int.e i] ]
    | Simple (n, Array(Some (Const s), ty)) when Inspect.is_char ty ->
      main[%expr Ctypes.string_from_ptr [%e start @@ get_field' n]
          [%e (var s).e] ]
    | Simple(n, (Option Array(Some (Path _ | Math_expr _ as p), inty)
                | Array(Some (Path _ | Math_expr _ as p),inty) as t)) ->
      let index = array_index get_field' types fields p in
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
          [%expr maybe (may (fun i a -> [%e a]) i) a]
        else if opt then
          [%expr may (fun i -> [%e a]) i ]
        else if aopt then
          [%expr may (fun a -> [%e a]) a ]
        else a in
      main [%expr let a = [%e get_field' n]
        and i = [%e index] in [%e body] ]
    | Simple(name,_) -> main @@ get_field (varname name)
  end


let rec ty_of_int ty x =
  match ty with
  | Ty.Option t -> [%expr Option.Some [%e ty_of_int t x]]
  | Name t -> [%expr [%e ident @@ qn t "of_int"] [%e x] ]
  | _ -> x
let array_len x = [%expr Ctypes.CArray.length [%e x] ]

let nullptr = function
  | Ty.Option _ -> [%expr None]
  | t -> C.coerce C.(ptr void) (Ast__type.converter true t) [%expr Ctypes.null]
(*   | ty -> not_implemented "Null array not implemented for %a@." Ty.pp ty *)



let set types typ r field value =
  let name = varname % fst and ty = snd in
  let array_len (_, ty as _index) = ty_of_int ty (array_len value.e) in
  let optzero f = if Inspect.is_option (ty f) then [%expr None] else [%expr 0] in
  let setf f x = setf r f x in
  match field with
  | Ty.Simple(f, (Ptr Name t | Const Ptr Name t)) when Inspect.is_record types t ->
    setf (varname f) (C.addr value.e)
  | Ty.Simple(f, (Option (Const Ptr Name t | Ptr Name t)
                 | Const Option(Const Ptr Name t | Ptr Name t))) when
      Inspect.is_record types t ->
    setf (varname f) [%expr may Ctypes.addr [%e value.e]]
  | Ty.Simple(f, Array(Some (Lit n), t)) when Inspect.is_char t ->
    setf (varname f) [%expr convert_string [%e int.e n] [%e value.e]]
  | Ty.Simple(f, Array(Some (Const n), t)) when Inspect.is_char t ->
    setf (varname f) [%expr convert_string [%e ex var n] [%e value.e]]
  | Ty.Simple(f, Array(Some (Path _| Math_expr _), _)) ->
    setf (varname f) (start value.e)
  | Ty.Simple(f, Option Array(Some (Path _|Math_expr _), _)) ->
    setf (varname f) [%expr may Ctypes.CArray.start [%e value.e] ]
  | Ty.Simple (f,_ty) ->
    setf (varname f) value.e
  | Ty.Array_f { index; array } as t when Inspect.is_option_f t ->
    [%expr match [%e value.e] with
      | Option.None ->
        [%e setf (name index) (optzero index)];
        [%e setf  (name array) (nullptr @@ ty array)]
      | Option.Some [%p value.p] ->
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
        [%e Ast__record_extension.split (typ,exts) value.e ] in
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
      | Some Ty.Bitfields _ -> pp (Ast__bitset.set_name t)
      | _ -> pp t end
  | Array(_, Name t) when L.to_path t = ["char"] ->[%expr pp_string]
  | Array(_,t) -> [%expr pp_array [%e printer t]]
  | Option t -> [%expr pp_opt [%e printer t]]
  | Const t -> printer t
  | Ptr t when Inspect.is_void t -> [%expr pp_addr]
  | Ptr t -> [%expr pp_ptr [%e printer t]]
  | String -> [%expr pp_string]
  | ty -> Fmt.epr "Not implemented: %a@." Ty.pp ty;
    raise @@ Invalid_argument "Printer not implemented"

let seq  = C.listr (fun x y -> [%expr [%e x]; [%e y]])

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
      (val' ~:"pp"[%type: Printer.formatter -> t -> unit])
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
  let seal = hidden [%stri let () = Ctypes.seal t] in
  match kind with
  | Union -> module' inner (structure @@ imap (sfield types) fields) ^:: seal
  | Record ->
    let lens f = getter typename types fields f in
    let exts= match Inspect.record_extension fields with
      | Some exts -> Ast__record_extension.def (typename,exts)
      | None -> nil in
    exts
    @*  module' inner
      (structure @@ List.fold_right (fun x l -> field types x @* l) fields nil)
    ^:: seal @* (fold_map lens fields) @* pp types fields ^:: nil

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
        let [%p array.p] = Ctypes.CArray.of_list t [%e input.e] in
        [%e keep]
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
    [%expr let [%p res.p] = Ctypes.make t in
      [%e keep_alive @@ seq set fields res.e ]
    ] in
  item [%stri let make = [%e fn body]]
    (val' ~:"make" @@ Ast__type.fn types ~regular_struct:true ~with_label:true tyname fields [%type: t])

let make (type a) types (kind: a kind) (name, fields: _ * a list) =
  let records = match kind with
    | Union -> imap (union types) fields
    | Record -> construct types name fields ^:: nil in
  module' name
    (structure @@ def types (name,kind) name fields @* array ^:: records)
  ^:: item
    [%stri let [%p pat var name] = [%e ident @@ qn name "t"]]
    (val' name @@ [%type: [%t typ ~par:name ~:"t"] Ctypes.typ])
  ^::  C.extern_type name
  ^:: nil
