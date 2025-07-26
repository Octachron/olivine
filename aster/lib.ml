module L = Info.Linguistic
type path = L.name list
module I = Item
module U = Utils

type ast_item = (Parsetree.structure, Parsetree.signature) I.item
module Deps = Set.Make(struct type t = path let compare = compare end)

module T = Info.Refined_types
module Cty = T.Ty
module Arith = T.Arith

module Ty = Rename.Ty
module LOrd = struct type t = L.name let compare = compare end
module Name_set = Set.Make(LOrd)
module Name_map = Map.Make(LOrd)

type 'a with_deps = { x:'a; deps: Deps.t }

type const = Arith.t
type typedef' = Ty.typedef
type fn = Ty.fn
type implementation = Raw | Regular | Native

module M = Info.Common.StringMap

type module' = {
  name : L.name;
  path: path;
  args: (string * Parsetree.module_type) list;
  sig': item list
}

and item =
  | Const of L.name * const
  | Fn of { implementation: implementation; fn:fn }
  | Type of L.name * typedef'
  | Module of module'
  | Ast of ast_item
and sig' = item M.t

let rec is_empty m =
  let empty_submodule  = function
    | Module m -> is_empty m
    | Ast _  -> true
    | _ -> false in
  m.sig' = [] || List.for_all empty_submodule m.sig'




module Result = struct
  module Map = Map.Make(struct
      type t = Ty.name
      let compare = compare
    end)
  let make constrs =
    List.fold_left
      (fun m (x,n) -> match n with
         | T.Abs n -> Map.add x n m
         | _ -> m) Map.empty constrs
end

type lib = {
  content: module';
  result: int Result.Map.t;
  builtins: Name_set.t
}

type context = {
  types: typedef' Name_map.t;
  current: L.name list;
  builtins: Name_set.t;
  results: int Result.Map.t;
}


type full_dict =
  { aliases: string M.t;
    dict: L.dict
  }

let rename full_dict =
  let ling = L.make full_dict.dict in
  fun name ->
    let fresh =
      match M.find name full_dict.aliases with
      | x -> x
      | exception Not_found -> name in
    ling fresh

type kind = Typedef | Builtin | Result | Prim


let make ?(args=[]) ?(sig'=[]) path name =
  { name; path; sig'; args }


let find_type name {types; _ } =
  Name_map.find_opt name types

let (#?) ctx name = find_type name ctx

let context ~builtins ~results current items =
  let rec types m l = List.fold_left
        (fun map -> function
           | Type (n,ty) -> Name_map.add n ty map
           | Module m  -> types map m.sig'
           | _ -> map
        ) m l in
  { current; builtins; results;
    types = types Name_map.empty items
  }

let rec find_module name = function
  | [] -> None
  | Module m :: _ when m.name = name -> Some m
  | _ :: q -> find_module name q


let item_name = function
  | Module m -> Some m.name
  | Type (n, _) -> Some n
  | Fn f -> Some f.fn.name
  | Const (n,_) -> Some n
  | Ast _ -> None


let update_assoc key default f items =
  let rec search l =
  function
  | Module m :: q when m.name = key ->
    Some ( List.rev_append l @@ Module(f m) :: q )
  | a :: q -> search (a :: l) q
  | [] -> None in
  match search [] items with
  | None -> Module (f default) :: items
  | Some items -> items

let is_defined item =
  let same_definition item item' = match item, item' with
    | Const (n,_), Const(n', _)
    | Type (n, _), Type(n',_)  ->  n = n'
    | Module m , Module m' -> m.name = m'.name
    | Fn f, Fn f' -> f.fn.name = f'.fn.name
    | _ -> false in
  List.exists (same_definition item)

let add path item module' =
  let rec add prefix rest module' =
    match rest with
    | [] ->
      let s = module'.sig' in
      let sig' = if is_defined item s then s else item :: s in
      { module' with sig' }
    | w :: q ->
      { module' with
        sig' =
          update_assoc w
            (make prefix w)
            (add (w::prefix) q)
            module'.sig'
      } in
  add [] path module'

let add_l path item lib =
  { lib with content = add path item lib.content }

module S = Info.Common.StringSet

let may f = function
  | None -> None
  | Some x -> Some (f x)

let subresult = L.simple ["subresult"]

let rec dep_typ (dict,gen as g) (items,lib as build) =
  function
  | Cty.Ptr t | Const t | Option t | Array(_,t) | Width{ty=t; _ } ->
    dep_typ g build t
  | Name t ->
    if S.mem t items then gen build t else build
  | Result _ as t ->
    begin match Rename.typ (rename dict) t with
      | Rename.Ty.Result {ok;bad} as t ->
        let name = Info.Subresult.composite_nominal ok bad in
        let okname = Info.Subresult.side_name ok in
        let badname = Info.Subresult.side_name bad in
        items,
        lib
        |> add [subresult] @@ Type (okname, Ty.(Alias (Result {ok; bad=[]})))
        |> add [subresult] @@ Type (badname, Ty.(Alias (Result {bad; ok=[]})))
        |> add [subresult] @@ Type (name, Ty.Alias t)
      | _ -> assert false
    end
  | FunPtr {return; _ } -> dep_typ g build return
  | String -> build


let dep_fields gen fields build =
  List.fold_left
      ( fun acc (_,t) -> dep_typ gen acc t )
      build fields

let dep_fn gen (fn: Cty.fn) build =
  fn.return |> dep_typ gen build |> dep_fields gen
    (Cty.flatten_fn_fields fn.args)

(** Once the name of the function analyzed, it become easier to identify
    out parameter *)
let refine_fn (fn:Ty.fn) =
  let rec identify_out (args:Ty.fn_field list) =
    match args with
    | [] -> []
    | [ { field = Simple (_, Ptr _); _ } as f ] ->
       [{f with dir = Out }]
    | [ {field = Ty.Simple (_,Ty.Array(Some(Path _), Name _ )); _ } as f] ->
      [{ f with dir = Out }]
    | a :: q -> a :: identify_out q in
  match L.to_path fn.name with
  | ("create" | "get" | "enumerate"|"allocate"|"acquire") :: _
  | "map" :: "memory" :: _
    ->
    { fn with args = identify_out fn.args }
  | _ -> fn

let deps gen build = function
  | Cty.Alias t ->
    dep_typ gen build t
  | Handle _ | Enum _ | Bitfields _ -> build
  | Union fields ->
    dep_fields gen fields build
  | Record {fields; _ } ->
    dep_fields gen (Cty.flatten_fields fields) build
  | Bitset { field_type = Some name'; _ } ->
    snd gen build name'
  | Bitset _ -> build

let result_info dict registry =
  match M.find "VkResult" registry with
  | exception Not_found -> assert false
  | Info.Entity.Type Cty.Enum constrs ->
    Result.make @@ List.map Rename.(constr @@ rename dict) constrs
  | _ -> assert false

let const = L.simple["const"]
let core = L.simple["core"]
let raw = L.simple["raw"]
let types = L.simple["types"]

let raw_builtins =
  [ "uint_32_t";  "cametallayer"; "uint_16_t"; "void"; "int_32_t"; "uint_64_t";
   "int_64_t"; "size_t"; "uint_8_t"; "float"; "double"; "int" ]


let builtins dict =
  Name_set.of_list @@
  L.simple ["bool";"32"]
  :: List.map (rename dict) raw_builtins

(** Remove [p] from [items] and extend the module tree [lib] with it. *)
let rec generate_ideal core dict registry current
    (items, lib as build) p =
  if not @@ S.mem p items then build else
  let (items, lib as build ) = (S.remove p items, lib) in
  let name = rename dict p in
  let renamer = rename dict in
  match M.find_opt p registry with
  | Some Info.Entity.Const c ->
    let lib = add [const] (Const (name,c)) lib in
    items,lib
  | Some Info.Entity.Fn fn ->
    let items, lib =
      dep_fn (dict, generate_ideal core dict registry current)
        fn build in
    let fn = refine_fn @@ Rename.fn renamer fn in
    let lib =
      if Ty.is_simple fn then
        lib
        |> add ( core current ) (Fn { implementation=Regular; fn})
        |> add (current @ [raw]) (Fn {implementation=Raw; fn})
      else
        lib
        |> add ( core current ) (Fn {implementation=Native; fn})
        |> add (current @ [raw]) (Fn {implementation=Raw; fn}) in
    items, lib
  | Some Info.Entity.Type typ ->
    let items, lib =
      deps (dict,generate_ideal core dict registry current) build
        typ in
    let lib = match typ with
      | Alias Name n when List.mem n raw_builtins -> lib
      | _ ->
        let typ = Rename.typedef renamer typ in
        match typ with
        | Bitfields _ as t -> let rname = U.bitset_core_name name in
          lib |> add [types; rname] (Type (name, t))
        | _ ->
          lib |> add [types; name] (Type (name,typ))
    in
    (items,lib)
  | None -> Fmt.epr "Lost item %s@." p;
    (* FIXME *)
    (items,lib)


let rec generate_core core dict registry path (items, _ as build) =
  if items = S.empty then
    snd build
  else
    let p = S.choose items in
    generate_core core dict registry path
    @@ generate_ideal core dict registry path build p

let rec normalize_sigs acc = function
  | Module m :: q ->
    let m = normalize m in
    begin if is_empty m && m.args = [] then
      normalize_sigs acc q
    else
      normalize_sigs (Module m :: acc) q
  end
  | a :: q ->
    normalize_sigs (a::acc) q
  | [] -> acc
and normalize m =
  { m with sig' = normalize_sigs [] m.sig' }

let classify_extension dict m (ext:Info.Structured_extensions.t) =
  let path = L.to_path @@ rename dict ext.metadata.name in
  match path with
  | [] -> assert false
  | a :: _ ->
    let exts = try M.find a m with Not_found -> [] in
    M.add a (ext::exts) m

let vk = L.simple ["vk"]

let rec take_module name = function
  | [] -> make [name] name , []
  | Module a :: q when a.name = name -> a, q
  | a :: q -> let m, q = take_module name q in m, a :: q



let open' s =
  let open Ast_helper in
  let name = Utils.nlid @@  " Vk__" ^ s  in
  let opn = Opn.mk name in
  let m = Opn.mk (Mod.ident name) in
   I.item (Str.open_ m) (Sig.open_ opn)

let opens x = I.imap open' x
let ast x = Ast x
let loc = Location.none
let mkforeign prefix name =
  let sig' = [ast @@
   I.item
     [%str let libvulkan = Dl.dlopen ~filename:"libvulkan.so"
               ~flags:Dl.[RTLD_NOW]
           let foreign name = Foreign.foreign ~from:libvulkan name]
     []
   ] in
  make prefix name ~sig'


let core_submodules prefix =
  List.map (fun m -> Module m)
    [mkforeign prefix core;
     mkforeign prefix raw;
     make prefix subresult;
     make prefix types]


let generate_subextension dict registry branch lib
    (ext:Info.Structured_extensions.t) =
  let renamer = rename dict in
  let name = List.rev (renamer ext.metadata.name).postfix in
  let name = L.simple(L.remove_prefix [branch] name) in
  if Sys_info.is_specific name then lib else
  match ext.metadata.type' with
  | None -> lib
  | Some t ->
    let vkext = "Vk__extension_sig" in
    let s = I.str (U.modtype ~par:L.[simple [vkext]] @@ renamer t) in
    let args = ["X", s] in
    let preambule =
      I.item
        [U.include' @@ U.Me.apply
           U.(nloc @@ Longident.Lident vkext / ("Foreign_"^t)) "X"]
        []
    in
    let items = S.of_list
      @@ List.filter (fun name -> not @@ Sys_info.is_specific @@ renamer name)
      @@ ext.commands (*@ ext.types*) in
    let branch' = L.simple [branch] in
    let ext_m =
      make ~args ~sig':( Module (mkforeign [] raw) ::
                        [Ast preambule])
        []  name in
    let lib = add [branch'] (Module ext_m) lib in
    generate_core (fun x -> x) dict registry [branch';name]
      (items, lib)

let generate_extensions dict registry extensions lib =
  let exts =
    List.fold_left (classify_extension dict) M.empty
      (Info.Structured_extensions.only_active extensions) in
  let gen_branch name exts lib =
    List.fold_left (generate_subextension dict registry name)
      lib exts in
(*    in
    Module ( normalize @@ make ~sig':submodules [vk]
             @@ L.simple [name])
      :: acc in*)
   M.fold gen_branch exts lib

let filter_extension dict registry name0 =
  let name = L.make dict name0 in
  match M.find name0 registry with
  | Info.Entity.Type _ -> not @@ Sys_info.is_specific name
  | Fn _ ->
    not (L.is_extension dict name|| Sys_info.is_specific name)
  | Const _ -> true



(*
let find_submodule name lib =
  List.find (fun m -> m.name = name) lib.content.submodules
*)

(* Create the tree of modules to be generated. *)
let generate dict (spec:Info.Structured_spec.spec) =
  let registry = spec.entities in
  let submodules = core_submodules [vk] in  (* Skeleton Vk.{Core,Raw,Subresult,Types} *)
  let items =
       S.of_list
    @@ List.filter (filter_extension dict registry)
    @@ List.map fst
    @@ M.bindings registry in
  let dict = {aliases = Info.Structured_spec.aliases spec; dict } in
  let content =
    normalize
    @@ generate_extensions dict registry spec.extensions
    @@ generate_core
      ( fun path -> path @ [core] )
      dict registry [] (items, make ~sig':submodules [] vk) in
  { result = result_info dict registry; content;
    builtins = builtins dict}
