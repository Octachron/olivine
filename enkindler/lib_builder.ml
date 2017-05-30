module L = Name_study
type path = string list

module Deps = Set.Make(struct type t = path let compare = compare end)

module T = Ctype
module Cty = T.Ty
module Arith = T.Arith

module Full_name = struct
  type name = L.name
  let pp ppf x = L.pp_var ppf x
end
module Ty = Ctype.Typexpr(Full_name)
module Name_set = Set.Make(struct type t = L.name let compare = compare end)

type 'a with_deps = { x:'a; deps: Deps.t }

type const = Arith.t
type type' = Ty.typexpr
type fn = Ty.fn

module M = Map.Make(String)

type module' = {
  name : string;
  path: path;
  preambule: string;
  args: string list;
  sig': (L.name * item) list;
  submodules: module' list
}

and item =
  | Const of const
  | Fn of { simple: bool; fn:fn }
  | Type of type'

and sig' = item M.t

let rec is_empty m =
    m.sig' = [] && List.for_all is_empty m.submodules

let sys_specific =
  let module S = Misc.StringSet in
  let all = S.of_list
      [ "xlib";  "xcb";  "wl"; "android"; "wayland"; "mir"; "win"; "win32" ] in
  let unsupported = S.diff all @@ S.of_list Config.supported_systems in
  let check x = S.mem x unsupported in
  fun name -> List.exists (List.exists check )
    Name_study.[name.prefix;name.postfix;name.main]

module Result = struct
  module Map = Map.Make(struct type t = Ty.name let compare = compare end)
  let make constrs =
    List.fold_left
      (fun m (x,n) -> match n with
         | T.Abs n -> Map.add x n m
         | _ -> m) Map.empty constrs
end

type lib = {
  content: module';
  result: int Result.Map.t;
  root: string;
  preambule: string;
  builtins: Name_set.t
}

let make ?(args=[]) ?(submodules=[]) ?(sig'=[]) ?(preambule="") path name =
  { name; path; sig'; args; submodules; preambule }

let rec update_assoc key default f = function
  | m :: q when m.name = key -> (f m) :: q
  | a :: q -> a :: update_assoc key default f q
  | [] -> [f default]

    (*
let find_module name at m =
    match List.assoc name m.submodules with
      | exception Not_found ->
        let s = make name at in
        s, (name,s) :: m.submodules
      | s -> s, m.submodules
*)

let add path name item module' =
  let rec add prefix rest module' =
  match rest with
    | [] ->
      let s = module'.sig' in
      let sig' = if List.mem_assoc name s then s else (name,item) :: s in
      { module' with sig' }
  | w :: q ->
    { module' with
      submodules =
        update_assoc w (make prefix w) (add (w::prefix) q) module'.submodules
    } in
  add [] path module'

let add_l path name item lib =
  { lib with content = add path name item lib.content }

module S = Misc.StringSet

let may f = function
  | None -> None
  | Some x -> Some (f x)

module Rename = struct
  let elt dict = L.make dict
  let rec typ (!) x =
    let typ = typ (!) in
    match x with
    | Cty.Name n -> Ty.Name !n
    | Const ty -> Ty.Const(typ ty)
    | Ptr ty -> Ty.Ptr(typ ty)
    | Option ty  -> Ty.Option(typ ty)
    | String -> String
    | Handle p ->
      Ty.Handle { parent = may (!) p.parent; dispatchable=p.dispatchable }
    | Array (cexpr, t) -> Ty.Array( may (const (!)) cexpr, typ t)
    | FunPtr f -> Ty.FunPtr (fn (!) f)
    | Enum constrs -> Ty.Enum(List.map (constr(!)) constrs)
    | Union f -> Ty.Union (sfields (!) f)
    | Record r ->
      Ty.Record{ is_private = r.is_private; fields = fields (!) r.fields }
    | Bitset b ->
      Ty.Bitset { implementation = ! (b.implementation);
                  field_type = may (!) b.field_type }
    | Bitfields b ->
      Ty.Bitfields { fields = List.map (bitfield (!)) b.fields;
                     values =  List.map (bitfield (!)) b.values
                   }
    | Result r -> Ty.Result{ ok = List.map (!) r.ok;
                             bad = List.map (!) r.bad }
    | Record_extensions l ->
      Ty.Record_extensions (record_extension (!) l)
  and const (!) = function
    | Cty.Lit a -> Ty.Lit a
    | Path p -> Ty.Path (List.map (!) p)
    | Const n -> Ty.Const !n
    | Null_terminated -> Ty.Null_terminated
    | Math_expr -> Ty.Math_expr
  and fn (!) {Ctype.Ty.args; name; original_name; return } =
    Ty.{ args = fn_fields (!) args;
         name = ! name; original_name;
         return = typ (!) return }
  and bitfield (!) (name, n) = !name, n
  and sfield (!) (n,ty) = !n, typ (!) ty
  and field (!) = function
    | Cty.Simple f -> Ty.Simple(sfield (!) f)
    | Array_f r ->
      Array_f { index = sfield (!) r.index;
                array = sfield (!) r.array }
    | Record_extension { exts; tag; ptr } ->
      Record_extension {
        exts = record_extension (!) exts;
        tag = sfield (!) tag;
        ptr = sfield (!) ptr
      }
  and fn_field (!) (r:Cty.fn_field) =
    { Ty.dir = r.dir; field = field (!) r.field }
  and sfields (!) = List.map @@ sfield (!)
  and fields (!) = List.map @@ field (!)
  and fn_fields (!) = List.map @@ fn_field (!)
  and constr (!) (n, p) = !n, p
  and record_extension (!) l =
    List.filter (fun x -> not @@ sys_specific x)
    @@ List.map (!) l
end

let rec dep_typ (dict,gen as g) (items,lib as build) =
  function
  | Cty.Ptr t | Const t | Option t | Array(_,t) ->
    dep_typ g build t
  | Name t ->
    if S.mem t items then gen build t else build
  | Result _ as t ->
    begin match Rename.typ (L.make dict) t with
      | Ty.Result {ok;bad} as t ->
        let name = Subresult.composite_nominal ok bad in
        let okname = Subresult.side_name ok in
        let badname = Subresult.side_name bad in
        items,
        lib
        |> add ["subresult"] okname (Type (Ty.Result {ok; bad=[]}))
        |> add ["subresult"] badname (Type (Ty.Result {bad; ok=[]}))
        |> add ["subresult"] name (Type t)
      | _ -> assert false
    end
  | Record_extensions exts ->
    List.fold_left (dep_typ g) build @@ List.map (fun n -> Cty.Name n)
      exts
  | _ -> build


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
  match Name_study.to_path fn.name with
  | ("create" | "get" | "enumerate"|"allocate"|"acquire") :: _
  | "map" :: "memory" :: _
    ->
    { fn with args = identify_out fn.args }
  | _ -> fn

let deps gen build = function
  | Cty.Option t | Ptr t | Array(_,t) | (Name _ as t) ->
    dep_typ gen build t
  | Const _  | String
  | Result _  | Handle _ | Enum _ | Bitfields _ -> build
  | FunPtr fn -> dep_fn gen fn build
  | Union fields ->
    dep_fields gen fields build
  | Record {fields; _ } ->
    dep_fields gen (Cty.flatten_fields fields) build
  | Bitset { field_type = Some name'; _ } ->
    snd gen build name'
  | Bitset _ -> build
  | Record_extensions exts ->
    List.fold_left (dep_typ gen) build @@ List.map (fun n -> Cty.Name n)
    exts

let result_info dict registry =
  match M.find "VkResult" registry with
  | exception Not_found -> assert false
  | Typed.Type Cty.Enum constrs ->
    Result.make @@ List.map Rename.(constr @@ elt dict) constrs
  | _ -> assert false

let rec generate_ideal dict registry (items, lib as build) p =
  if not @@ S.mem p items then build else
  let (items, lib as build ) = (S.remove p items, lib) in
  let name = L.make dict p in
  let renamer = Name_study.make dict in
  match M.find p registry with
  | Typed.Const c ->
    let lib = add ["const"] name (Const c) lib in
    items,lib
  | Typed.Fn fn ->
    let items, lib = dep_fn (dict, generate_ideal dict registry) fn build in
    let fn = refine_fn @@ Rename.fn renamer fn in
    let lib =
      if Ty.is_simple fn then
        lib |> add ["core"] name (Fn { simple=true; fn})
      else
        lib
        |> add ["core"] name (Fn {simple=false; fn})
        |> add ["raw"] name (Fn {simple=true; fn}) in
    items, lib
  | Typed.Type typ ->
    let items, lib = deps (dict,generate_ideal dict registry) build typ in
    let typ = Rename.typ renamer typ in
    let lib =
      lib |> add ["types"] name (Type typ) in
    (items,lib)

let rec generate_core dict registry (items, _ as build) =
  if items = S.empty then
    snd build
  else
    let p = S.choose items in
    generate_core dict registry
    @@ generate_ideal dict registry build p

let rec normalize m =
  { m with submodules = List.rev_map normalize m.submodules;
           sig' = List.rev m.sig' }

let classify_extension dict m (ext:Typed.Extension.t) =
  let path = L.to_path @@ L.make dict ext.metadata.name in
  match path with
  | [] -> assert false
  | a :: _ ->
    let exts = try M.find a m with Not_found -> [] in
    M.add a (ext::exts) m

let generate_subextension dict registry branch l (ext:Typed.Extension.t) =
  let name = List.rev (L.make dict ext.metadata.name).postfix in
  let name = L.simple(L.remove_prefix [branch] name) in
  if sys_specific name then l else
  match ext.metadata.type' with
  | None -> l
  | Some t ->
    let args = [Format.asprintf "X:%s" t] in
    let preambule = Format.asprintf "\ninclude Foreign_%s(X)\n" t in
    let items = S.of_list
      @@ List.filter (fun name -> not @@ sys_specific @@ L.make dict name)
      @@ ext.commands (*@ ext.types*) in
    let mname = Format.asprintf "%a" L.pp_module name in
    let ext_m = make ~args ~preambule ["vk"; branch]  mname in
    let m = generate_core dict registry (items, ext_m) in
    begin if List.length m.submodules > 3 then
        ( List.iter (fun m -> Fmt.epr "Submodule:%s@." m.name) m.submodules;
          assert false
        )
    end;

    let map = List.fold_left (fun m x -> M.add x.name x m) M.empty m.submodules in
       let (#.) map n = try [M.find n map] with Not_found -> [] in
    let sig' = List.fold_left (fun acc x -> x.sig' @ acc) [] map#."core"  in
    let submodules = map#."raw" @ map#."subresult" in
    { m with
      submodules;
      sig'
    } :: l

let opens l = String.concat "" @@ List.map (fun s -> "open Vk__" ^ s ^ "\n") l

let generate_extensions dict registry extensions =
  let exts = List.fold_left (classify_extension dict) M.empty extensions in
  let preambule = opens [ "const"; "types"; "extension_sig"; "subresult"] in
  let gen_branch name exts acc =
    let submodules =
      List.fold_left (generate_subextension dict registry name) [] exts in
        (normalize @@ make ~preambule ~submodules ["vk"] name) :: acc in
   M.fold gen_branch exts []

let filter_extension dict registry name0 =
  let name = L.make dict name0 in
  match M.find name0 registry with
  | Typed.Type _ -> not @@ sys_specific name
  | Fn _ ->
    not (L.is_extension dict name|| sys_specific name)
  | Const _ -> true

let builtins dict =
  Name_set.of_list @@ List.map (L.make dict) ["vkBool32"]


let find_submodule name lib =
  List.find (fun m -> m.name = name) lib.content.submodules

let generate root preambule dict (spec:Typed.spec) =
  let registry = spec.entities in
  let submodules =
    let raw_open = ["const"; "types"; "subresult"] in
    [make ~preambule:(opens @@ raw_open @ ["raw"]) ["vk"] "core";
     make ~preambule:(opens raw_open) ["vk"] "raw";
     make ~preambule:( opens ["const"] ^ "include Builtin_types\n") ["vk"] "types";
    ] in
  let items =
       S.of_list
    @@ List.filter (filter_extension dict registry)
    @@ List.map fst
    @@ M.bindings registry in
  let content =
    normalize @@ generate_core dict registry (items, make ~submodules [] "vk") in
  let content =
    { content with
      submodules = content.submodules
                   @ generate_extensions dict registry spec.extensions
    } in
  { root; preambule; result = result_info dict registry; content;
    builtins = builtins dict}
