
module Aliases= struct
  module L = Info.Linguistic
  module B = Lib
  module Ty = B.Ty
  module H = Ast_helper
  module Exp = H.Exp
  module P = Parsetree
  module C = Common
  module M = Info.Common.StringMap
end
open Aliases
open Item
open Utils

(*   let debug fmt = Fmt.epr ( "Debug:"^^ fmt ^^ "@.%!") *)

let unique, reset_uid = C.id_maker ()

let addr_f ctx = C.addrf ctx

let make_f t exp =
  [%expr [%e ident(qn t "unsafe_make")] [%e exp] ]

let ($) f x = [%expr [%e f] [%e x]]

let regularize_type types = function
  | Ty.Ptr (Name t as n) | Const Ptr (Name t as n)
    when Inspect.is_record types t ->
    n
  | ty -> ty

let may f x = [%expr Vk__helpers.may [%e f] [%e x] ]

let regularize types ty exp= match ty with
  | Ty.Ptr Name t | Const Ptr Name t when Inspect.is_record types t ->
    addr_f types t $ exp <?> "Regularized pointer to struct"
  | Option Ptr Name t| Const Option Ptr Name t
    when Inspect.is_record types t -> may (addr_f types t) exp
    <?> "Regularized optional pointer to struct"
  | (Ptr Option Name t|Const Ptr Option Name t)
    when Inspect.is_record types t -> may (addr_f types t) exp
    <?> "Regularized pointer to optional struct"
  (*  | Option Ptr Name _ -> may (C.addr exp)*)
  | ty ->  (exp <?> "No regularization: %a") Ty.pp ty


let regularize_fields types fields =
  let reg f =
    match f.Ty.field with
    | Simple(n, (Ptr Name t | Const Ptr Name t))
      when Inspect.is_record types t ->
      { f with field = Simple(n, Name t) }
    | Simple(n, Option Ptr Name t)
      when Inspect.is_record types t ->
      { f with field = Simple(n, Option (Name t)) }
    | _ -> f
  in
  List.map reg fields

let annotate fmt =
  Fmt.kstr (fun s e -> Exp.attr e @@
              (H.Attr.mk (nloc "debug") (P.PStr [H.Str.eval @@ string s ])))
    fmt

let arg_types (fn:Ty.fn) =
  Funptr.expand @@ List.map snd @@ Ty.flatten_fn_fields fn.args

let foreign types fn =
  let args = arg_types fn in
  [%expr foreign [%e string fn.original_name] [%e
      Funptr.mkty types args fn.return]]

let make_simple types (f:Ty.fn) =
  item
    [[%stri let [%p (var f.name).p] = [%e foreign types f] ]]
    [val' f.name @@ Type.fn ~decay_array:All types
       f.name (List.map (fun ty -> Ty.Simple ty) @@ Ty.flatten_fn_fields f.args)
       (Type.mk ~raw_type:true ~decay_array:All types f.return)]

let apply_gen ?attrs get name vars args =
  let get f = Asttypes.Nolabel, get vars f in
  let add_arg l = function
    | Ty.Array_f { array ; index } ->
      (get array) :: (get index) :: l
    | Simple field -> get field :: l
    | Record_extension _  -> assert false in
  let args = List.rev @@ List.fold_left add_arg [] args in
  Exp.apply ?attrs name args

let apply = apply_gen
    (fun vars (f,_ty) -> ex (M.find @@ varname f) vars)


let get_r types vars (f,ty) =
  regularize types ty @@ ex (M.find @@ varname f) vars


let apply_regular ?attrs types = apply_gen ?attrs (get_r types)

let mkfn_simple fields =
  let build (f,vars) (name,_ty) =
    let u = unique (varname name) in
    (fun body -> f [%expr fun [%p u.p] -> [%e body] ]),
    M.add (varname name) u vars in
  List.fold_left build ((fun x -> x), M.empty) fields

let make_regular types fn =
  let args = Inspect.to_fields fn.Ty.args in
  let f = unique "f" in
  let def body =
    [%stri let [%p pat var fn.name] =
             let [%p f.p] = [%e foreign types fn] in
             [%e body] ] in
  item
    [ def begin
          let fe, vars = mkfn_simple @@ Ty.flatten_fields args in
          fe @@ apply_regular types f.e vars args
        end
    ]
    [val' fn.name @@ Type.fn types
       ~regular_struct:true
       fn.name (List.map (fun ty -> Ty.Simple ty) @@ Ty.flatten_fields args)
       (Type.mk types ~regular_struct:true fn.return)
    ]

let make_labelled types m fn =
  let args = Inspect.to_fields fn.Ty.args in
  let k, vars = Structured.mkfun args in
  item
    [%stri let make =
             [%e k @@ apply (ident @@ qn m @@ varname fn.name) vars args]
    ]
    [val' fn.name @@ Type.fn2 types ~regular_struct:true ~with_label:true fn]


module Option = struct
  let map f = function
    | None -> None
    | Some x -> Some (f x)
end

let rec ptr_to_name ?(ellide=true) = function
  | Ty.Option t ->
    Option.map (fun (ty,name) -> (Ty.Option ty, name))
      (ptr_to_name ~ellide t)
  | Ty.Name t -> Some(Ty.Name t, t )
  | Ty.Ptr p | Array(_,p) ->
    Option.map (fun (p,elt) -> if ellide then (p,elt) else (Ty.Ptr p, elt) )
    @@ ptr_to_name ~ellide:false p
  | _ -> None

let nullptr_typ p = [%expr nullptr [%e p]]
let allocate_n ty n = [%expr Ctypes.allocate_n [%e ty] [%e n]]
let allocate ty value = [%expr Ctypes.allocate [%e ty] [%e value]]


(* Allocate composite fields *)
let allocate_field types fields vars f body  =
  let pre = Inspect.prefix (fun ?(par=[]) x ->
      ident (qualify par @@ varname x)) types in
  let get f = M.find (varname f) vars in
  match f.Ty.field with
  | Simple(f, Array(Some p , Name elt)) ->
    let array = get f in
    let size =  get @@ C.index_name f in
    let n =
      Structured.array_index ~conv:false types (ex get)
        fields p in
    [%expr let [%p size.p] = [%e n ] in
      let [%p array.p ] =
        (Ctypes.allocate_n[@olivine.info "allocate_field Simple"])
          [%e pre elt] [%e size.e ] in
      [%e body]
    ]
  | Simple(f, Option _ ) ->
    let f = get f in
    [%expr let [%p f.p ] = None in [%e body] ]
  | Simple(f, Name t) when Inspect.is_record types t ->
    let f = get f in
    [%expr let [%p f.p ] =
             [%e Structured.unsafe_make types  t] in [%e body] ]
  | Simple (f,t) ->
    begin let f = get f in
      match ptr_to_name t with
      | None -> body
      | Some (ty,_) ->
        let alloc = C.wrap_opt t @@ allocate_n
            (Type.converter types ~degraded:true ty) [%expr 1] in
        [%expr let [%p f.p] = [%e alloc] in [%e body] ]
    end
  | Array_f { array=a, Option _; index=i, Ptr Option Name t } ->
    let a = get a and i = get i in
    let name = Inspect.prefix varpath types t
        ~name:(L.simple ["ctype";"opt"]) in
    [%expr let [%p i.p] = Ctypes.allocate
               [%e ident name]
               None in
      let [%p a.p] = None in [%e body]
    ]
  | Array_f { array=a, elt; index=i, size } ->
    let a = get a and i = get i in
    begin match ptr_to_name elt, ptr_to_name size with
      | None, _ | _, None -> body
      | Some (e,_), Some(s,_) ->
        let alloc_size = C.wrap_opt size @@ allocate_n
            (Type.converter types ~degraded:true s) [%expr 1]
        and alloc_elt = nullptr_typ
            (Type.converter types ~degraded:true e) in
        [%expr let [%p a.p] = [%e alloc_elt]
          and [%p i.p] = [%e alloc_size] in
          body
        ]
    end
  | _ -> C.not_implemented "Native function for field type %a" Ty.pp_fn_field f

(* Array output parameter needs to be allocated in two times:
   first the index parameter is allocated,
   then the function is applied and fills the actual value of the
   index parameter, which enable us to allocate the output array *)
let secondary_allocate_field types vars f body = match f.Ty.field with
  | Array_f { array = a, tya; index = i, it  } ->
    let a = M.find (varname a) vars and i = M.find (varname i) vars in
    let size = Structured.int_of_ty types i.e it in
    begin match ptr_to_name tya with
      | Some (Option elt, _ ) ->
        let alloc = allocate_n
            (Type.converter types ~degraded:true elt) size in
        [%expr let [%p a.p] = [%e C.wrap_opt tya @@ alloc] in [%e body] ]
      | Some _ | None -> assert false end
  | _ -> body

let extract_opt input output nullptr map body =
  let scrutinee = unique "scrutinee" in
  [%expr let [%p output.p] = match [%e input] with
      | None -> None, [%e nullptr]
      | Some [%p scrutinee.p] -> [%e map scrutinee.e] in  [%e body]
  ]

let len' x = [%expr Ctypes.CArray.length [%e x] ]
let len types ty x = Structured.ty_of_int types ty @@ len' x

let start = Structured.start
let extract_array ctx input (ty,index) array body =
  [%expr
    let [%p index] = [%e len ctx ty input] in
    let [%p array] = [%e start input] in
    [%e body]
  ]

let (<*>) x y =
  { p = [%pat? [%p x.p], [%p y.p] ]; e = [%expr [%e x.e], [%e y.e] ] }

let nullptr = Nullable.ptr

let input_expand types vars f body = match f with
  | Ty.Array_f { array= (a, tya ) ; index = (i, ty )  } as f ->
    let a = M.find (varname a) vars and i = M.find (varname i) vars in
    if Inspect.is_option_f f then
      let extract_array input =
        [%expr [%e len types ty input], [%e start input]] in
      extract_opt a.e ( i <*> a )
        (nullptr types tya) extract_array body
    else
      extract_array types a.e (ty,i.p) a.p body
  | Simple(f, Array(Some Path _, ty)) ->
    let f = M.find (varname f) vars in
    if Inspect.is_option ty then
      [%expr let [%p f.p] = match [%e f.e] with
          | Some [%p f.p] -> Some [%e start f.e]
          | None -> None in [%e body]
      ]
    else
      [%expr let [%p f.p] =[%e start f.e] in [%e body] ]
  | Simple(f, Option(Array(Some Path _, _))) ->
    let f = M.find (varname f) vars in
      [%expr let [%p f.p] = match [%e f.e] with
          | Some [%p f.p] -> Some [%e start f.e]
          | None -> None in [%e body]
      ]
  | Simple(f, Array(Some (Const _ | Lit _),_) )->
    let f = M.find (varname f) vars in
    [%expr let [%p f.p] = [%e start f.e] in [%e body] ]
  | _ -> body

let tuple l = Exp.tuple l
let unwrap x = [%expr Vk__helpers.unwrap [%e x] ]


let ty_of_int = Structured.ty_of_int
let int_of_ty = Structured.int_of_ty
let from_ptr x y = [%expr Ctypes.CArray.from_ptr [%e x] [%e y] ]
let to_output types vars f =
  let get x = ex (M.find @@ varname x) vars in
  match f.Ty.field with
  | Ty.Array_f { array = (n, Ty.Option _) ; index = i , it } ->
    from_ptr (unwrap @@ get n) (int_of_ty types (get i) it)
  | Ty.Array_f { array = (n,_) ; _ } -> get n
  | Simple(f, Array(Some(Path _), Name _ )) ->
    from_ptr (get f)
      (ex (M.find @@ varname @@ C.index_name f) vars)
  | Simple(n, Name t)  when Inspect.is_record types t ->  get n
  | Simple(n, Ptr Option _) ->
    unwrap [%expr (Ctypes.(!@) [%e get n]) ]
  | Simple(n, _) -> [%expr Ctypes.(!@) [%e get n] ]
  | Record_extension _ ->
    C.not_implemented "Record extension used as a function argument"

let join ty res outputs =
  let n = List.length outputs in
  if Inspect.is_result ty then
    let u = unique "ok" in
    let outputs = if outputs = [] then [[%expr ()]] else outputs in
    [%expr match [%e res] with
      | Error _ as e -> e
      | Ok [%p u.p] -> Ok [%e tuple @@ u.e :: outputs]
    ]
  else if  n = 0 then
    res
  else if Inspect.is_void ty then
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
      u.e :: l, vars |> M.add (varname n) v |> M.add (varname @@ C.index_name n) u
    | f ->
      let u = unique (varname @@ C.repr_name f) in
      u.e :: l, vars |> M.add (varname @@ C.repr_name f) u
  ) ([], vars) output

let result_part ok bad =
  polyvariant_type ~order:Eq @@ List.map nloc @@ List.map mkconstr ok,
  polyvariant_type ~order:Eq @@  List.map nloc @@  List.map mkconstr bad


let return_type types outputs return =
  let output_typ = function
    | Ty.Simple(_, Ptr Option Ptr ty ) -> Ty.Ptr ty
    | Ty.Simple(_, (Option Ptr ty |Const Option Ptr ty | Ptr ty)) -> ty
    | Ty.Array_f { array = (_, Ty.Option ty) ; index = _ } ->
      ty
    | Ty.Array_f { array = (_,ty) ; _ } -> ty
    | Simple(_, Option ty) -> ty
    | Simple(_, (Array _ as ty) ) -> ty
    | Simple(_, (Name _ as ty)) -> ty
    | ty -> Format.eprintf "Impossible type for output argument: %a@."
              Ty.pp_field ty;
      exit 2 in
  let mkty ty = Type.mk ~strip_option:true ~regular_struct:true types
      (output_typ ty) in
  match return with
  | Ty.Result {ok;bad}  ->
    let ok, bad = result_part ok bad in
    let ty =
      match outputs with
      | [] -> [%type: unit]
      | [a] -> mkty a
      | l -> H.Typ.tuple (List.map mkty l) in
    [%type: ([%t ok] * [%t ty], [%t bad]) Pervasives.result ]
  | a when Inspect.is_void a->
    begin match outputs with
      | [] -> [%type: unit]
      | [a] -> mkty a
      | l -> H.Typ.tuple (List.map mkty l)
    end
  | _ ->
    let return = match return with Option ty -> ty | ty -> ty in
    let return = Type.mk ~regular_struct:true types return
                 <?:> "Direct return type" in
    begin match outputs with
      | [] -> return
      | l -> H.Typ.tuple ( return :: List.map mkty l)
    end

let raw ctx = if Inspect.in_extension ctx then
    ~:"Raw" else ~:"Vk__Raw"

let make_native types (fn:Ty.fn)=
  reset_uid ();
  let rargs = regularize_fields types fn.args in
  let fold f l body = List.fold_right ((@@) f ) l body in
  let input, output =
    List.partition (fun r -> r.Ty.dir = In || r.dir = In_Out) rargs in
  let apply_twice = List.exists
      (function { Ty.field = Array_f _ ; _ } -> true | _ -> false)
      output in
  let tyret = fn.return in
  let input' = Inspect.to_fields input in
  let all = Inspect.to_fields fn.args in
  let fun', vars = Structured.mkfun input' in
  let _, vars = look_out vars output in
  item [
  (fun x -> [%stri let [%p pat var fn.name] = [%e x] ]) @@
  fun' @@
  fold (input_expand types vars) input' @@
  fold (allocate_field types input' vars) output @@

  let apply =
    (apply_regular ~attrs:[info "make_native"]
      types (ident @@ qualify [raw types] @@ varname fn.name)
      vars all
      <?>
      "fn.apply: context:[%a]/%a in extension %B" )
      (Fmt.list L.full_pp) (types.B.current)
      L.pp_module (raw types)
      (Inspect.in_extension types)
  in
  let res =
    if Inspect.is_void tyret then
      Utils.any
    else unique "res" in
  let result =
    let outs = List.map (to_output types vars) output in
    [%expr let [%p res.p] = [%e apply] in [%e join tyret res.e outs] ] in
  let secondary = fold (secondary_allocate_field types vars) output in
  if not apply_twice then
    result
  else if Inspect.is_result fn.return then
    [%expr match [%e apply] with
      | Error _ as e -> e
      | Ok _ -> [%e secondary result]
    ]
  else
    [%expr [%e apply]; [%e secondary result] ]
]
    [val' fn.name @@ Type.fn types ~regular_struct:true ~with_label:true
       fn.name input' (return_type types (Inspect.to_fields output) tyret)]

let make types = function
  | B.Regular -> make_regular types
  | Native -> make_native types
  | Raw -> make_simple types
