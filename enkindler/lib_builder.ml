module L = Name_study
type path = L.name list
module I = Ast__item
  module U = Ast__utils
type ast_item = (Parsetree.structure, Parsetree.signature) I.item
module Deps = Set.Make(struct type t = path let compare = compare end)

module T = Retype
module Cty = T.Ty
module Arith = T.Arith

module Full_name = struct
  type name = L.name
  let pp ppf x = L.pp_var ppf x
end
module Ty = Retype.Typexpr(Full_name)
module Name_set = Set.Make(struct type t = L.name let compare = compare end)

type 'a with_deps = { x:'a; deps: Deps.t }

type const = Arith.t
type type' = Ty.typexpr
type fn = Ty.fn
type implementation = Raw | Regular | Native

module M = Map.Make(String)

type module' = {
  name : L.name;
  path: path;
  args: string list;
  sig': item list
}

and item =
  | Const of L.name * const
  | Fn of { implementation: implementation; fn:fn }
  | Type of L.name * type'
  | Module of module'
  | Ast of ast_item
and sig' = item M.t

let rec is_empty m =
  let empty_submodule  = function
    | Module m -> is_empty m
    | _ -> false in
  m.sig' = [] || List.for_all empty_submodule m.sig'

let sys_specific =
  let module S = Enkindler_common.StringSet in
  let all = S.of_list
      [ "xlib";  "xcb";  "wl"; "android"; "wayland"; "mir"; "win"; "win32" ] in
  let unsupported = S.diff all @@ S.of_list Econfig.supported_systems in
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
  preambule: ast_item ;
  builtins: Name_set.t
}

let make ?(args=[]) ?(sig'=[]) path name =
  { name; path; sig'; args }


let rec find_type name = function
  | [] -> None
  | Type(n,t) :: _ when n = name -> Some t
  | _ :: q -> find_type name q


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
  | Module m :: q when m.name = key -> Some ( List.rev_append l @@ Module(f m) :: q )
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
          update_assoc w (make prefix w) (add (w::prefix) q) module'.sig'
      } in
  add [] path module'

let add_l path item lib =
  { lib with content = add path item lib.content }

module S = Enkindler_common.StringSet

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
    | Math_expr x -> Ty.Math_expr x
  and fn (!) {Cty.args; name; original_name; return } =
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

let subresult = L.simple ["subresult"]

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
        |> add [subresult] @@ Type (okname, Ty.Result {ok; bad=[]})
        |> add [subresult] @@ Type (badname,Ty.Result {bad; ok=[]})
        |> add [subresult] @@ Type (name,t)
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

let const = L.simple["const"]
let core = L.simple["core"]
let raw = L.simple["raw"]
let types = L.simple["types"]

let rec generate_ideal dict registry (items, lib as build) p =
  if not @@ S.mem p items then build else
  let (items, lib as build ) = (S.remove p items, lib) in
  let name = L.make dict p in
  let renamer = Name_study.make dict in
  match M.find p registry with
  | Typed.Const c ->
    let lib = add [const] (Const (name,c)) lib in
    items,lib
  | Typed.Fn fn ->
    let items, lib = dep_fn (dict, generate_ideal dict registry) fn build in
    let fn = refine_fn @@ Rename.fn renamer fn in
    let lib =
      if Ty.is_simple fn then
        lib
        |> add [core] (Fn { implementation=Regular; fn})
        |> add [raw] (Fn {implementation=Raw; fn})
      else
        lib
        |> add [core] (Fn {implementation=Native; fn})
        |> add [raw] (Fn {implementation=Raw; fn}) in
    items, lib
  | Typed.Type typ ->
    let items, lib = deps (dict,generate_ideal dict registry) build typ in
    let typ = Rename.typ renamer typ in
    let lib =
      lib |> add [types] (Type (name,typ)) in
    (items,lib)

let rec generate_core dict registry (items, _ as build) =
  if items = S.empty then
    snd build
  else
    let p = S.choose items in
    generate_core dict registry
    @@ generate_ideal dict registry build p

let rec normalize m =
  let sub = function Module m -> Module (normalize m) | x -> x in
  { m with sig' = List.rev_map sub m.sig' }

let classify_extension dict m (ext:Typed.Extension.t) =
  let path = L.to_path @@ L.make dict ext.metadata.name in
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
  let opn = Opn.mk @@ Ast__utils.nlid @@  " Vk__" ^ s ^ "\n" in
   I.item (Str.open_ opn) (Sig.open_ opn)

let opens x = I.imap open' x
let ast x = Ast x

let core_submodules =
  let raw_open = ["const"; "types"; "subresult"] in
  List.map (fun m -> Module m)
    [make ~sig':[ast @@ opens @@ raw_open @ ["raw"]] [vk] core;
     make ~sig':[ast @@ opens raw_open] [vk] raw;
     make [vk] subresult]


let generate_subextension dict registry branch l (ext:Typed.Extension.t) =
  let name = List.rev (L.make dict ext.metadata.name).postfix in
  let name = L.simple(L.remove_prefix [branch] name) in
  if sys_specific name then l else
  match ext.metadata.type' with
  | None -> l
  | Some t ->
    let args = [Format.asprintf "X:%s" t] in
    let preambule =
      I.item
        [U.include' @@ U.Me.apply ("Foreign_"^t) "X"]
        []
in
    let items = S.of_list
      @@ List.filter (fun name -> not @@ sys_specific @@ L.make dict name)
      @@ ext.commands (*@ ext.types*) in
    let branch' = L.simple [branch] in
    let ext_m = make ~args ~sig':(core_submodules @ [Ast preambule])
        [vk; branch']  name in
    let m = generate_core dict registry (items, ext_m) in
 (*   begin if List.length m.submodules > 3 then
        ( List.iter (fun m -> Fmt.epr "Submodule:%s@." m.name) m.submodules;
          assert false
        )
      end;*)
    let core, rest = take_module core m.sig' in
    Module { m with sig' = core.sig' @ rest } :: l

let generate_extensions dict registry extensions =
  let exts = List.fold_left (classify_extension dict) M.empty extensions in
  let preambule = opens [ "const"; "types"; "extension_sig"; "subresult"] in
  let gen_branch name exts acc =
    let submodules =
      List.fold_left (generate_subextension dict registry name) [] exts in
    Module ( normalize @@ make ~sig':(submodules @ [ast preambule]) [vk]
             @@ L.simple [name])
    :: acc in
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

(*
let find_submodule name lib =
  List.find (fun m -> m.name = name) lib.content.submodules
*)

let generate root preambule dict (spec:Typed.spec) =
  let registry = spec.entities in
  let submodules =
    let sig' = let open I in
      [ ast @@
        opens ["const"] @* item [[%stri include Builtin_types]]
          [%sig: include (module type of Builtin_types) open Unsigned ]
      ] in
    core_submodules
    @ [ Module (make ~sig' [vk] types) ] in
  let items =
       S.of_list
    @@ List.filter (filter_extension dict registry)
    @@ List.map fst
    @@ M.bindings registry in
  let content =
    normalize @@ generate_core dict registry (items, make ~sig':submodules [] vk) in
  let content =
    { content with
      sig' = content.sig'
                   @ generate_extensions dict registry spec.extensions
    } in
  { root; preambule; result = result_info dict registry; content;
    builtins = builtins dict}
