module B = Lib_builder
module T = B.T
module Ty = B.Ty
module L = Name_study
module Arith = B.Arith

let is_bits name =
  match name.L.postfix with
  | "bits" :: _  -> true
  | _ -> false

let pp_type builtins results types ppf (name,ty) =
  Pprintast.structure ppf
  @@ match ty with
  | Ty.Const _  | Option _ | Ptr _ | String | Array (_,_) -> []
  | Result {ok;bad} ->
    Aster.Result.make results (name,ok,bad)
  | Name t -> Aster.alias builtins (name,t)
  | FunPtr fn -> [Aster.Funptr.make (name,fn)]
  | Union fields -> Aster.Structured.make types Union (name,fields)
  | Bitset { field_type = Some _; _ } -> []
  | Bitset { field_type = None; _ } -> Aster.Bitset.make (name,None)
  | Bitfields {fields;values} ->
    Aster.Bitset.make_extended (name,(fields,values))
  | Handle _ ->  Aster.Handle.make name
  | Enum constrs ->
    if not @@ is_bits name then
      begin
        let is_result = name.main = ["result"] in
        let kind = if is_result then Aster.Enum.Poly else Aster.Enum.Std in
        Aster.Enum.make kind (name,constrs)
      end
    else []
  | Record r ->
    Aster.Structured.make types Record (name,r.fields)
  | Record_extensions _ -> (* FIXME *)
    assert false

let pp_item (lib:B.lib) ppf (name, item) =
  let types = (B.find_submodule "types" lib).sig' in
  match item with
  | B.Type t -> pp_type lib.builtins lib.result types ppf (name,t)
  | Const c -> Pprintast.structure ppf [Aster.Const.make (name,c)]
  | Fn f -> Pprintast.structure ppf [Aster.Fn.make types f.simple f.fn]


let pp_open ppf m =
  if m.B.args = [] then
    Fmt.pf ppf "open %s@." (String.capitalize_ascii m.B.name)

let rec pp_module lib ppf (m:B.module') =
  Fmt.pf ppf
    "@[<v 2>module %s%a= struct@;%a@;end@]@;%a@;"
    (String.capitalize_ascii m.name)
    pp_args m.args
    (pp_sig lib) m
    pp_open m
and pp_sig lib ppf (m:B.module') =
    Fmt.pf ppf "%a@;%a@;"
      (Fmt.list @@ pp_module lib) m.submodules
      (Fmt.list @@ pp_item lib) m.sig'

and pp_args ppf = function
  | [] -> ()
  | args -> Fmt.list pp_arg ppf args
and pp_arg ppf arg = Fmt.pf ppf "(%s)" arg

let atlas ppf modules =
  let pp_alias ppf (m:B.module') =
    Fmt.pf ppf "module %s = Vk__%s@;"
      (String.capitalize_ascii m.name) m.name
  in
  Fmt.pf ppf "@[<v>%a@]@." (Fmt.list pp_alias) modules

let lib (lib:B.lib) =
  let open_file n =
    Format.formatter_of_out_channel @@ open_out @@ lib.root ^ "/" ^ n in
  let  pp_preambule ppf (m:B.module') =
    Fmt.pf ppf "%s\n%s\n" lib.preambule m.preambule in
  atlas (open_file "vk.ml") lib.content.submodules;
  let pp_sub (m:B.module') =
    if not (B.is_empty m) then
      begin
        let ppf = open_file ("vk__" ^ m.name ^ ".ml") in
        Fmt.pf ppf "%a\n%a%!"
          pp_preambule m
          (pp_sig lib) m
      end in
  List.iter pp_sub lib.content.submodules
