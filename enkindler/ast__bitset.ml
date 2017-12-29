module Aliases = struct
  module L = Name_study
  module C = Ast__common
  module Pat = Ast_helper.Pat
end open Aliases

open Ast__item
open Ast__utils


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
let field = named [%type: 'a set] field_name [%expr make_index]
let value = named [%type: t]  value_name [%expr of_int]

let values set_name (fields,values) =
  imap (field set_name) fields
  @* imap (value set_name) values

let pp set (fields,_) =
  let field (name,_) =
    let name = field_name set name in
    [%expr [%e ex var name], [%e string (varname name)]] in
  let l = C.listr (fun x l -> [%expr [%e x] :: [%e l] ]) field fields [%expr []] in
  item [%stri let pp x = pp_tags [%e l] x]
    [%sigi: val pp: Printer.formatter -> 'a set -> unit]

let resume bitname name =
  let index_view b ty n =
    let v = Pat.var (nloc b) and e = ident (qn name n) in
    item [%stri let [%p v] = [%e e ]]
      (val' ~:b [%type: [%t ty] Ctypes.typ ]) in
  let ty = typ ~par:[name] ~:"index" in
  C.extern_type name ^:: C.views name
  @* index_view bitname ty "index_view"
  ^:: index_view (bitname^"_opt") [%type: [%t ty] option] "index_view_opt"
  ^:: nil

let make_extended (bitname, fields) =
  let name = set_name bitname in
  let core_name = let open L in
    { name with postfix = List.filter (fun x -> x <> "flags") name.postfix }
  in
  let values = values core_name fields in
    (module' name @@ structure @@
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
