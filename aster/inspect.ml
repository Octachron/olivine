module Aliases= struct
  module L = Info.Linguistic
  module B = Lib
  module Ty = B.Ty
end
open Aliases
module U=Utils

let in_types ctx = [U.type_module] = ctx.B.current

let in_extension ctx =
  let t x = ctx.B.current = [L.simple x] in
  not @@ (t ["core"] || t ["raw"])

let is_result_name x = L.to_path x = ["result"]
let is_option_f = function
  | Ty.Simple (_, (Option _ | Const Option _) )
  | Ty.Array_f { index = _, (Option _  | Const Option _ ); _ }
  | Ty.Array_f { array = _, (Option _ | Const Option _); _ }
  | Ty.Record_extension _ -> true
  | _ -> false

let is_option = function
  | Ty.Option _ | Const Option _ -> true
  | _ -> false

let is_char = function
  | Ty.Name t -> L.to_path t = ["char"]
  | _ -> false

(*
let is_result = function
  | B.Ty.Result _ -> true
  | _ -> false
*)

let typeclass name (ctx:B.context) =
  let rec root name = match name.L.postfix with
    | "opt" :: postfix -> root { name with postfix }
    | _ -> name in
  let name = root name in
  if B.Result.Map.mem name ctx.results then B.Result else
    match B.find_type name ctx,
          B.Name_set.mem name ctx.builtins
    with
    | _ , true -> B.Builtin
    | Some _, false -> B.Typedef
    | None, false -> B.Prim


let vk_prefix ctx name =
  if in_extension ctx then
    name
  else
    "Vk__" ^ name

let (~:) = L.(~:)
let bty = ~: "Vk__builtin__types"

let prefix ?(prim=[]) ?(name= ~:"t") tyvar ?(par=[]) ctx rootname =
  match typeclass rootname ctx with
  | B.Typedef ->
    tyvar
      ?par:(Some((U.tymod rootname)::par)) name
  | B.Builtin ->
    tyvar
      ?par:(Some(bty :: rootname :: par))
      name
  | B.Result ->
    tyvar
      ?par:(Some(~:"Vk__Subresult"::par)) name
  | B.Prim ->
    tyvar ?par:(Some(prim @ par)) rootname


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

let typeclass name ctx =
  match B.find_type name ctx, B.Name_set.mem name ctx.builtins
  with
  | Some t, _ -> if is_result t then B.Result else B.Typedef
  | None, true -> B.Builtin
  | None, false -> B.Prim

let find_record tn ctx =
  match B.find_type tn ctx with
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
  let rec type_path types acc (ty, fields) = function
    | [] -> acc
    | a :: q ->
      match find_field_type a fields with
      | Some (Ty.Const Ptr Name tn| Ptr Name tn|Name tn as tyo) ->
        begin match q with
          | [] -> (ty,tyo,a) :: acc
          | _ :: _ ->
            let direct = match tyo with Name _ -> true | _ -> false in
            let fields = find_record tn types in
            type_path types ((ty,tyo,a) :: acc)
              (Some (direct,tn),fields) q
        end
      | Some ty ->
        (None,ty,a) :: acc
      (*          Fmt.epr "Path ended with %a@.%!" Ty.pp ty;
                  raise @@ Invalid_argument "Non-record path" *)
      | _ ->
        Format.eprintf "Fatal error: unknow type path %a@." L.pp_type a;
        exit 2 in
  type_path types [] (None, fields) p

let rec last_type = function
  | [] -> assert false
  | [_,ty,_] -> ty
  | _ :: q -> last_type q

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
