module B = Aster.Lib
module Ty = B.Ty
module L = Info.Linguistic
module I = Aster.Item
module U = Aster.Utils

let is_bits name =
  match name.L.postfix with
  | "bits" :: _  -> true
  | _ -> false

let item, str, sg = I.(item,str,sg)


let pps =
  item (fun ppf x -> Pprintast.structure ppf x)
    (fun ppf x -> Pprintast.signature ppf x)
let ppi = item (fun ppf x -> str pps ppf [x])
    (fun ppf x -> sg pps ppf [x])

type d = (Parsetree.structure,Parsetree.signature) I.item


type side = Str | Sig
let pp_file_extension ppf = function
  | Str -> Fmt.pf ppf ".ml"
  | Sig -> Fmt.pf ppf ".mli"

let map side f x = match side with
  | Str -> (I.str f) (I.str x)
  | Sig -> (I.sg f) (I.sg x)

let print side f ppf x =
  match side with
  | Str -> I.(str f ppf @@ str x)
  | Sig -> I.(sg f ppf @@ sg x)

module Atlas_set =
  Set.Make(struct type t = L.name list let compare = compare end)

let delim = Atlas_set.(
    empty |> add L.[~:"vk"] |> add  L.[ ~:"vk"; ~:"types" ]
  )

let rec type_to_ast ctx (name,ty) =
  match ty with
  | Ty.Const _  | Option _ | Ptr _ | String | Array (_,_) -> I.nil
  | Result {ok;bad} ->
    Aster.Result.make ctx (name,ok,bad)
  | Name t -> Aster.Misc.alias ctx (name,t)
  | FunPtr fn -> Aster.Funptr.make ctx (name,fn)
  | Union fields -> Aster.Structured.make ctx Union (name,fields)
  | Bitset { field_type = Some _; _ } -> I.nil
  | Bitset { field_type = None; _ } -> Aster.Bitset.make (name,None)
  | Bitfields {fields;values} ->
    Aster.Bitset.make_extended (name,(fields,values))
  | Handle {dispatchable;_} ->  Aster.Handle.make ~dispatchable name
  | Enum constrs ->
    if not @@ is_bits name then
      begin
        let is_result = name.main = ["result"] in
        let kind =
          if is_result then Aster.Enum.Poly
          else Aster.Enum.Std in
        Aster.Enum.make kind (name,constrs)
      end
    else I.nil
  | Record r ->
    Aster.Structured.make ctx Record (name,r.fields)
  | Record_extensions _ -> (* FIXME *)
    assert false
  | Width w ->
    type_to_ast ctx (name,w.ty)

let rec item_to_ast current (lib:B.lib) item =
  let types = match B.find_module B.types lib.content.sig' with
    | Some m -> m.sig'
    | None -> [] in
  let ctx = B.context ~builtins:lib.builtins
      ~results:lib.result current types in
  I.rev @@ match item with
  | B.Type (name,t) ->
    type_to_ast ctx (name,t)
  | Const (name,c) -> Aster.Misc.Const.make (name,c)
  | Fn f -> Aster.Fn.make ctx f.implementation f.fn
  | Ast s -> s
  | Module m -> module_to_ast current lib m
and module_to_ast path lib (m:B.module') =
  let s x = [x] in
  I.fmap (item s s)
  @@ U.module' m.name
  @@ List.fold_left (fun sig' (name,mty) -> U.functor' name mty sig' )
  (U.structure
   @@ I.fold_map (item_to_ast (m.name::path) lib) m.sig')
  m.args

let pp_concrete_name =
  Fmt.list
    ~sep:(fun ppf () -> Fmt.pf ppf "__" )
    L.pp_module

let rec submodules = function
  | B.Module m :: q -> m :: submodules q
  | _ :: q -> submodules q
  | [] -> []

let atlas (close,ppfs) modules =
  let rec pp_alias delim current ppfs (m:B.module') =
    let path = current @ [m.name] in
    if Atlas_set.mem path delim then
      (
        let submodules = submodules m.sig' in
        Fmt.pf ppfs.I.structure "module %a = struct@ "
          L.pp_module m.name;
        Fmt.pf ppfs.I.signature "module %a: sig@ "
          L.pp_module m.name;
        List.iter (pp_alias delim path ppfs) submodules;
        Fmt.pf ppfs.I.structure "end@,";
        Fmt.pf ppfs.I.signature "end@,";
      )
    else (
      Fmt.pf ppfs.I.structure "module %a = %a@,"
        L.pp_module m.name pp_concrete_name path;
      Fmt.pf ppfs.I.signature "module %a = %a@,"
        L.pp_module m.name pp_concrete_name path
    )

  in
  Fmt.pf ppfs.I.structure "@[<v>";
  Fmt.pf ppfs.I.signature "@[<v>";
  List.iter (pp_alias delim L.[~:"vk"] ppfs) modules;
  Fmt.pf ppfs.I.structure "@]@.";
  Fmt.pf ppfs.I.signature "@]@.";
  close ()


let lib (lib:B.lib) =
  let open_file target n =
    let f = open_out
      @@ Fmt.strf "%s/%s%a" lib.root n pp_file_extension target in
    (fun () -> close_out f), Format.formatter_of_out_channel f in
  let open_files n =
    let cstr, structure = open_file Str n in
    let csig, signature = open_file Sig n in
    (fun () -> cstr(); csig ()), { I.structure ; signature} in
  atlas (open_files "vk") (submodules lib.content.sig');
  let rec pp_sub current (m:B.module') =
    if not (B.is_empty m) then
      begin
        let path = current @ [m.name] in
        if Atlas_set.mem path delim then
          List.iter (pp_sub path) (submodules m.sig')
        else begin
          let filename = Fmt.strf "%a" pp_concrete_name path in
          let close, ppfs = open_files filename in
          let ast =
            I.( lib.preambule @*
                I.fold_map (item_to_ast [m.name] lib) m.sig')
          in
          print Str pps (str ppfs) ast;
          print Sig pps (sg ppfs) ast;
          Fmt.pf (str ppfs) "@.";
          Fmt.pf (sg ppfs) "@.";
          close ();
        end
      end
    else Fmt.epr "Printing %a submodule@.%!" L.pp_var m.name
  in
  List.iter (pp_sub L.[~:"vk"]) @@ submodules lib.content.sig'
