module Aliases= struct
  module L = Name_study
  module B = Lib_builder
  module Ty = Lib_builder.Ty
  module H = Ast_helper
  module Inspect = Ast__inspect
  module C = Ast__common
end
open Aliases
open Ast__utils

let rec converter ~degraded x =
  let make = converter ~degraded in
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
    Ast__result.expr (ok,bad)
  | Record_extensions _ -> [%expr ptr void]
  (* ^FIXME^?: better typing? *)
  | FunPtr _ ->
    failwith "Not_implemented: funptr"

let rec mk
    ?(raw_type = false)
    ?(regular_struct=false)
    ?(decay_array=false)
    ?(strip_option=false)
    ?(mono=true)
    types t =
  let mk ?(strip_option=false) =
    mk ~raw_type ~decay_array ~regular_struct ~mono ~strip_option types in
  match t with
  | Ty.Const t -> mk ~strip_option t
  | Ptr Name n when regular_struct && Inspect.is_record types n ->
    [%type: [%t typ n]]
  | Name n ->
    let t = [%type: [%t typ n]] in
    begin match B.find_type n types with
      | None -> t
      | Some Ty.Bitfields _ ->
        let par = Ast__bitset.set_name n in
        typ ~par ~:"index"
      | Some Bitset _ ->
        if mono then
          typ ~par:n ~:"t"
        else
          H.Typ.constr (nloc @@ lid (modname n)/"set") [[%type: 'a]]
      | Some _ -> t
    end
  | Ptr ty -> [%type: [%t mk ty] Ctypes.ptr ]
  | Array ((None|Some (Path _ | Math_expr _ )),ty)
    when decay_array -> [%type: [%t mk ty] Ctypes.ptr ]
  | Array(Some _, t)
    when Inspect.is_char t && not raw_type ->   [%type: string]
  | Option ty ->
    if strip_option then mk ty else [%type: [%t mk ty] option ]
  | String -> [%type: string]
  | Array (Some _ , ty) ->
    [%type: [%t mk ty] Ctypes.CArray.t ]
  | Result {ok;bad} ->
    let ok = polyvariant_type ~closed:false @@ List.map mkconstr ok in
    let bad = polyvariant_type ~closed:false @@ List.map mkconstr bad in
    [%type: ([%t ok], [%t  bad]) result ]
  | Record_extensions _ -> [%type: unit Ctypes.ptr ]
  (* ^FIXME^?: better typing? *)
  | Array (_,ty) -> [%type: [%t mk ty] Ctypes.ptr]
  | FunPtr _ -> C.not_implemented "funptr type"
  | Enum _ | Record _ | Union _ | Bitset _ | Bitfields _
  | Handle _  ->
    failwith "Anonymous type"

let fn types  ?(regular_struct=false) ?(mono=true) ?(with_label=false)
    typename fields ret =
  let (->>) (l,x) r = H.Typ.arrow l x r in
  let label n f =
    if not with_label then Asttypes.Nolabel
    else if Inspect.is_option_f f then
      Asttypes.Optional (varname n)
    else
      Labelled (varname n) in
  let strip_option = with_label in
  let arg f = match f with
    | Ty.Array_f { array=n, ty; _ } ->
      label n f, mk types ~regular_struct ~strip_option ~mono ty
    | Simple(n,ty) as f ->
      label n f , mk types  ~regular_struct ~strip_option  ~mono ty
    | Record_extension _ ->
      label ~:"ext" f ,  typ (Ast__record_extension.name typename) in
  let ret = if List.exists Inspect.is_option_f fields then
      (Nolabel, [%type: unit] ) ->> ret
    else
      ret in
  List.fold_right (fun field f -> arg field ->> f ) fields ret
