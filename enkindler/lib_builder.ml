module L = Name_study
type path = string list

module Deps = Set.Make(struct type t = path let compare = compare end)

module Cty = Ctype.Ty
module Arith = Ctype.Arith

module Full_name = struct type name = L.name let pp = L.pp_var end
module Ty = Ctype.Typexpr(Full_name)


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
  | Fn of fn
  | Type of type'

and sig' = item M.t

let sys_specific name =
  let check =
    function "xlib" | "xcb" | "wl" |
             "android" | "mir" | "win32" -> true | _ -> false in
  List.exists
    (List.exists @@ fun w -> check (Name_study.canon w) )
    Name_study.[name.prefix;name.postfix;name.main]

module Result = struct
  module Map = Map.Make(struct type t = Ty.name let compare = compare end)
  let make constrs =
    List.fold_left
      (fun m (x,n) -> match n with
         | Ty.Abs n -> Map.add x n m
         | _ -> m) Map.empty constrs
end

type lib = {
  content: module';
  result: int Result.Map.t;
  root: string;
  preambule: string;
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

let add path name item lib =
  let rec add prefix rest module' =
  match rest with
  | [] -> { module' with sig' = (name, item) :: module'.sig' }
  | w :: q ->
    { module' with
      submodules =
        update_assoc w (make prefix w) (add (w::prefix) q) module'.submodules
    } in
  { lib with content = add [] path lib.content }

module S = Misc.StringSet

let may f = function
  | None -> None
  | Some x -> Some (f x)

module Rename = struct
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
    | Union f -> Ty.Union (fields (!) f)
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
  and const (!) = function
    | Cty.Lit a -> Ty.Lit a
    | Var v -> Ty.Var (!v)
    | Const n -> Ty.Const !n
    | Null_terminated -> Ty.Null_terminated
    | Math_expr -> Ty.Math_expr
  and fn (!) {Ctype.Ty.args; name; return } =
    Ty.{ args = List.map (field (!)) args;
         name = ! name;
         return = typ (!) return }
  and bitfield (!) (name, n) = !name, n
  and field (!) (n,ty) = !n, typ (!) ty
  and fields (!) = List.map (field (!))
  and constr (!) (n, p) = !n, pos p
  and pos = function
    | Cty.Abs n -> Ty.Abs n
    | Offset n -> Ty.Offset n
    | Bit n -> Ty.Bit n
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
  | _ -> build


let dep_fields gen fields build =
  List.fold_left
      ( fun acc (_,t) -> dep_typ gen acc t )
      build fields

let dep_fn gen (fn: Cty.fn) build =
  fn.return |> dep_typ gen build |> dep_fields gen fn.args


let deps gen build = function
  | Cty.Option t | Ptr t | Array(_,t) | (Name _ as t) ->
    dep_typ gen build t
  | Const _  | String
  | Result _  | Handle _ | Enum _ | Bitfields _ -> build
  | FunPtr fn -> dep_fn gen fn build
  | Union fields | Record {fields; _ } ->
    dep_fields gen fields build
  | Bitset { field_type = Some name'; _ } ->
    snd gen build name'
  | Bitset _ -> build

let record_result (name,ty) lib = match ty with
  | Ty.Enum constrs when  List.map L.canon name.L.main = ["result"] ->
    { lib with result = Result.make constrs }
  | _ -> lib

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
    if L.(is_extension dict name || sys_specific name) then
      build
    else
      let fn = Rename.fn renamer fn in
      let lib = add ["core"] name (Fn fn) lib in
      items,lib
  | Typed.Type typ ->
    if sys_specific name then build else
    let items, lib = deps (dict,generate_ideal dict registry) build typ in
    let typ = Rename.typ renamer typ in
    let lib =
      lib |> record_result (name,typ) |> add ["types"] name (Type typ) in
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

let generate root preambule dict registry =
  let submodules =
    [make ~preambule:"open Vk__const\nopen Vk__types\nopen Vk__subresult"
       ["vk"] "core";
     make ~preambule:"open Vk__const\n" ["vk"] "types";
    ] in
  let lib =
    { root; preambule; result = Result.Map.empty;
      content = make ~submodules [] "vk" } in
  let items = S.of_list @@ List.map fst @@ M.bindings registry in
  let lib = generate_core dict registry (items,lib) in
  { lib with content = normalize lib.content }
