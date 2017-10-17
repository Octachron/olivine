module B = Lib_builder
module T = B.T
module Ty = B.Ty
module L = Name_study
module Arith = B.Arith
module I = Ast__item


let is_bits name =
  match name.L.postfix with
  | "bits" :: _  -> true
  | _ -> false

let pp_item ppf dual =
  Pprintast.structure (I.str ppf) (I.str dual);
  Pprintast.signature (I.sg ppf) (I.sg dual)



let pp_str = Pprintast.structure

type d = (Parsetree.structure,Parsetree.signature) I.item
let str ppf (x: d) = Pprintast.structure ppf (I.str x)
let sg ppf (x : d) = Pprintast.signature ppf (I.sg x)

let dual_pp = I.item str sg



let pp_type side builtins results types ppf (name,ty) =
  let nil = I.nil in
  side ppf
  @@ match ty with
  | Ty.Const _  | Option _ | Ptr _ | String | Array (_,_) -> nil
  | Result {ok;bad} ->
    Aster.Result.make results (name,ok,bad)
  | Name t -> Aster.alias builtins (name,t)
  | FunPtr fn -> Aster.Funptr.make types (name,fn)
  | Union fields -> Aster.Structured.make types Union (name,fields)
  | Bitset { field_type = Some _; _ } -> nil
  | Bitset { field_type = None; _ } -> Aster.Bitset.make (name,None)
  | Bitfields {fields;values} ->
    Aster.Bitset.make_extended (name,(fields,values))
  | Handle {dispatchable;_} ->  Aster.Handle.make ~dispatchable name
  | Enum constrs ->
    if not @@ is_bits name then
      begin
        let is_result = name.main = ["result"] in
        let kind = if is_result then Aster.Enum.Poly else Aster.Enum.Std in
        Aster.Enum.make kind (name,constrs)
      end
    else nil
  | Record r ->
    Aster.Structured.make types Record (name,r.fields)
  | Record_extensions _ -> (* FIXME *)
    assert false


let pp_open ppf m =
  if m.B.args = [] then
    Fmt.pf ppf "open %a@." L.pp_module m.B.name

let space ppf () = Fmt.pf ppf "@;"

  let rec pp_item side (lib:B.lib) ppf item =
    let types = match B.find_module B.types lib.content.sig' with
      | Some m -> m.sig'
      | None -> raise (Invalid_argument "Printers.pp_item: Missing type module") in
    match item with
    | B.Type (name,t) ->
      pp_type side lib.builtins lib.result types ppf (name,t)
    | Const (name,c) -> pp_str ppf [Aster.Const.make (name,c)]
    | Fn f -> pp_str ppf [Aster.Fn.make types f.implementation f.fn]
    | Ast s -> side ppf s
    | Module m -> pp_module side lib ppf m
  and  pp_module side lib ppf (m:B.module') =
    Fmt.pf ppf
      "@[<v 2>module %a%a= struct@;%a@;end@]@;%a@;"
      L.pp_module m.name
      pp_args m.args
      (pp_sig side lib) m
      pp_open m
  and pp_sig side lib ppf (m:B.module') =
    Fmt.list ~sep:space (pp_item side lib) ppf m.sig'
and pp_args ppf = function
  | [] -> ()
  | args -> Fmt.list pp_arg ppf args
and pp_arg ppf arg = Fmt.pf ppf "(%s)" arg



let atlas ppf modules =
  let pp_alias ppf (m:B.module') =
    Fmt.pf ppf "module %a = Vk__%a@;"
      L.pp_module m.name L.pp_var m.name
  in
  Fmt.pf ppf "@[<v>%a@]@." (Fmt.list pp_alias) modules

let rec submodules = function
  | B.Module m :: q -> m :: submodules q
  | _ :: q -> submodules q
  | [] -> []

let lib (lib:B.lib) =
  let open_file n =
    Format.formatter_of_out_channel @@ open_out @@ lib.root ^ "/" ^ n in
  let  pp_preambule side ppf =
    Fmt.pf ppf "%a\n" side lib.preambule in
  atlas (open_file "vk.ml") (submodules lib.content.sig');
  let pp_sub (m:B.module') =
    if not (B.is_empty m) then
      begin
        let filename = Fmt.strf "vk__%a.ml" L.pp_var m.name in
        let ppf = open_file filename in
        Fmt.pf ppf "%t\n%a%!"
          (pp_preambule str)
          (pp_sig str lib) m
      end;
        (* temporary *)
    if m.name = L.simple ["types"] then
      let filename = Fmt.strf "vk__%a.mli" L.pp_var m.name in
      let ppf = open_file filename in
      Fmt.pf ppf "%t\n%a%!"
        (pp_preambule sg)
        (pp_sig sg lib) m
    else Fmt.epr "Printing %a submodule@.%!" L.pp_var m.name

  in
  List.iter pp_sub @@ submodules lib.content.sig'
