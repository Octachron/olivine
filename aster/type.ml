module Aliases= struct
  module L = Info.Linguistic
  module B = Lib
  module Ty = B.Ty
  module H = Ast_helper
  module C = Common
end
open Aliases
open Utils
let (#?) = B.(#?)

let typ ?par ?name = Inspect.prefix typ ?par ?name
let tyvar ?(post=[]) ?par types name =
  let name, post = match types#?(name) with
    | Some Bitfields _ ->
      Bitset.set_name name, "index" :: "ctype" :: post
    | _ -> name, "ctype" :: post in
  Inspect.prefix
    ~name:(L.simple post)
    ~prim:[L.simple ["Ctypes"]] tyvar ?par
    types name

let const_ident = ident % qualify L.[simple ["Vk__Const"] ] % varname

let mult factor const =
  if factor = 1 then
    const
  else
    [%expr [%e int.e factor] * [%e const]]


let rec converter types ~degraded x =
  let tyvar ?post = tyvar ?post types in
  let make = converter types ~degraded in
  match x with
  | Ty.Const t -> make t
  | Name n -> tyvar n
  | Ptr Name n | Ptr Const Name n ->
    [%expr Ctypes.ptr [%e tyvar n]]
  | Ptr ty -> [%expr Ctypes.ptr [%e make ty] ]
  | Option Name n -> tyvar ~post:["opt"] n
  | Option (Ptr typ) -> [%expr Ctypes.ptr_opt [%e make typ] ]
  | Option Array (Some Const {factor; name} ,typ) when not degraded ->
    [%expr Vk__helpers.array_opt [%e mult factor (var name).e] [%e make typ] ]
  | Option Array (Some (Lit n) ,typ) when not degraded ->
    [%expr Vk__helpers.array_opt [%e int.e n ] [%e make typ ]]
  | Option Array (_,t) -> [%expr Ctypes.ptr_opt [%e make t]]
  | Option String -> [%expr Ctypes.string_opt]
  | Option t -> Fmt.epr "Not implemented: option %a@." Ty.pp t; exit 2
  | String -> [%expr Ctypes.string]
  | Array (Some Const {factor;name} ,typ) when not degraded ->
    [%expr Ctypes.array [%e mult factor (const_ident name)] [%e make typ]]
  | Array (Some (Lit n) ,typ) when not degraded ->
    [%expr Ctypes.array [%e int.e n] [%e make typ]]
  | Array (_,typ) -> make (Ty.Ptr typ)
  | Enum _ | Record _ | Union _ | Bitset _ | Bitfields _
  | Handle _  ->
    failwith "Anonymous type"
  | Result {ok;bad} ->
    Result.expr types (ok,bad)
  | Record_extensions _ -> [%expr Ctypes.ptr Ctypes.void]
  (* ^FIXME^?: better typing? *)
  | FunPtr _ ->
    failwith "Not_implemented: funptr"


type decay = All | Dyn_array | None

let rec mk
    ?(raw_type = false)
    ?(regular_struct=false)
    ?(decay_array=None)
    ?(strip_option=false)
    ?(mono=true)
    types t =
  let mk ?(regular_struct=false) ?(strip_option=false) =
    mk ~raw_type ~decay_array ~regular_struct ~mono ~strip_option
      types in
  let typ ?par ?name = typ ?par ?name types in
  match t with
  | Ty.Const t -> mk ~regular_struct ~strip_option t
  | Ptr Name n when regular_struct && Inspect.is_record types n ->
    typ n
  | Name n ->
    let t = typ n in
    begin match B.find_type n types with
      | None -> t
      | Some Ty.Bitfields _ ->
        let root = Bitset.set_name n in
        typ ~name:(~:"index") root
      | Some Bitset _ when mono -> typ n
      | Some Bitset _ ->
          H.Typ.constr (nloc @@ lid (modname n)/"set") [[%type: 'a]]
      | Some _ -> t
    end
  | Ptr ty -> [%type: [%t mk ty] Ctypes.ptr ]
  | Array (_ , ty) when decay_array = All ->
    [%type: [%t mk ty] Ctypes.ptr ]
  | Array ((None|Some(Path _ | Math_expr _ )) ,ty)
    when decay_array = Dyn_array ->
    [%type: [%t mk ty] Ctypes.ptr ]
  | Array(Some (Lit _ | Const _ ), t)
    when Inspect.is_char t && not raw_type ->
    [%type: string]
  |  Option ty ->
    if strip_option then
      mk ~regular_struct ty
    else
      [%type: [%t mk ~regular_struct ty] option ]
  | String -> [%type: string]
  | Array (Some (Lit _ | Const _ ) , ty) ->
    [%type: [%t mk ty] Ctypes.CArray.t ]
  | Array (_, ty) ->
    [%type: [%t mk ty] Ctypes.CArray.t ]
  | Result {ok;bad} ->
    let ok =
      polyvariant_type ~order:Eq @@ List.map nloc @@  List.map mkconstr ok in
    let bad =
      polyvariant_type ~order:Eq @@  List.map nloc @@ List.map mkconstr bad in
    [%type: ([%t ok], [%t  bad]) Pervasives.result ]
  | Record_extensions _ -> [%type: unit Ctypes.ptr ]
  (* ^FIXME^?: better typing? *)
  | FunPtr _ -> C.not_implemented "funptr type"
  | Enum _ | Record _ | Union _ | Bitset _ | Bitfields _
  | Handle _  ->
    failwith "Anonymous type"

let fn types
    ?(decay_array=None)
    ?(regular_struct=false)
    ?(mono=true)
    ?(with_label=false)
    fname fields ret =
  let mkty = mk types ~decay_array ~regular_struct ~mono
      ~strip_option:with_label in
  let (->>) (l,x) r =
    H.Typ.arrow l x r in
  let label n f =
    if not with_label then Asttypes.Nolabel
    else if Inspect.is_option_f f then
      Asttypes.Optional (varname n)
    else
      Labelled (varname n) in
  let arg f = match f with
    | Ty.Array_f { array=n, ty; _ } ->
      label n f, mkty ty
    | Simple(n,ty) as f ->
      label n f , mkty ty
    | Record_extension _ ->
      label ~:"ext" f ,
      typ types (Record_extension.name fname)
  in
  let ret =
    if List.exists Inspect.is_option_f fields && with_label then
      (Nolabel, [%type: unit] ) ->> ret
    else
      ret in
  match fields with
  | [] -> (Nolabel, [%type: unit]) ->> ret
  | fields ->
    List.fold_right (fun field f -> arg field ->> f ) fields ret

let fn2 types ?(decay_array=None) ?(regular_struct=false) ?(mono=true)
    ?(with_label=false) (f:Ty.fn) =
  fn types  ~decay_array ~regular_struct ~mono ~with_label f.name
    (Inspect.to_fields f.args) (mk types f.return)
