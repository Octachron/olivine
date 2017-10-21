module Aliases = struct
  module L = Name_study
  module H = Ast_helper
  module Inspect = Ast__inspect
  module Ty = Lib_builder.Ty
end open Aliases
open Ast__utils
open Ast__item


let not_implemented fmt =
  Format.kasprintf (fun s -> raise (Invalid_argument s)) fmt

let list merge map acc =
  List.fold_left (fun acc x -> merge acc @@ map x) acc

let listr merge map l last =
  List.fold_right (fun x acc -> merge (map x) acc) l last

let coerce ~from ~to' value = [%expr Ctypes.coerce [%e from] [%e to'] [%e value]]
let ptr x = [%expr Ctypes.ptr [%e x] ]
let ptr_opt x = [%expr Ctypes.ptr_opt [%e x] ]
let void = [%expr void]
let addr x = [%expr Ctypes.addr [%e x] ]
let (!@) x = [%expr !@[%e x]]

let views ?(f=fun x ->x) name =
  let n = var name and no = L.(name//"opt") in
  let e = open' name [%expr view, view_opt ] in
  let t = typ ~par:name ~:"t" in
  item
  [[%stri let [%p n.p], [%p pat var no] = [%e e]]]
  [ val' name [%type: [%t f t] Ctypes.typ]; val' no [%type: [%t f t] option Ctypes.typ]]

let extern_type name =
  decltype ~manifest:(H.Typ.constr (nloc @@ qn name "t")  [])
    (typestr name)

let wrap_opt ty v =
  if Inspect.is_option ty then
    [%expr Option.Some [%e v]]
  else
    v

module M = Enkindler_common.StringMap
let id_maker () =
  let dict : int M.t ref = ref M.empty in
  let unique str =
    let c = match M.find_opt str !dict with Some x -> x | None -> 1 in
    dict := M.add str (1+c) !dict;
    let s =  str ^"'" ^ string_of_int c in
    { p = H.Pat.var (nloc s); e = H.Exp.ident (nlid s) } in
  let reset () = dict := M.empty in
  unique, reset

let module_name name =
  let rec rename = function
    |  "bits" :: "flag" :: q ->
      "flags" :: q
    | [] -> []
    | a :: q -> a :: rename q in
  L.{ name with postfix = rename name.postfix }

let repr_name = function
  | Ty.Array_f { array = a, _ ; _ } -> a
  | Simple(n, _ ) -> n
  | Record_extension _ -> ~:"ext"

let index_name f = L.(f//"size'")

let unwrap_opt_ty = function
  | Ty.Option ty -> ty
  | ty -> ty
