module L = Name_study
type path = string list


module Deps = Set.Make(struct type t = path let compare = compare end)

type 'a with_deps = { x:'a; deps: Deps.t }

type const = Ctype.num_expr
type type' = Ctype.typexpr
type fn = Ctype.fn

module M = Map.Make(String)

type module' = {
  name : string;
  path: path;
  args: module' list;
  sig': (L.name * item) list;
  submodules: (string * module') list
}

and item =
  | Const of const
  | Fn of fn
  | Type of type'

and sig' = item M.t

let make ?(args=[]) ?(submodules=[]) ?(sig'=[]) path name =
  { name; path; sig'; args; submodules }

let rec update_assoc key default f = function
  | (a,b) :: q when a = key -> (key, f b) :: q
  | a :: q -> a :: update_assoc key default f q
  | [] -> [key, f default]

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
  | [] -> { module' with sig' = (name, item) :: module'.sig' }
  | w :: q ->
    { module' with
      submodules =
        update_assoc w (make prefix w) (add (w::prefix) q) module'.submodules
    } in
  add [] path module'

module S = Misc.StringSet

let rec dep_typ (dict,gen as g) (items,lib as build) = function
  | Ctype.Ptr t | Const t | Option t | Array(_,t) ->
    dep_typ g build t
  | Name t ->
    if S.mem t items then gen build t else build
  | Result {ok;bad} as t ->
    items,
    add ["subresult"] (Subresult.composite_nominal dict ok bad) (Type t) lib
  | _ -> build


let dep_fields gen fields build =
  List.fold_left
      ( fun acc (_,t) -> dep_typ gen acc t )
      build fields

let dep_fn gen (fn: Ctype.fn) build =
  fn.return |> dep_typ gen build |> dep_fields gen fn.args

let deps gen build = function
  | Ctype.Option t | Ptr t | Array(_,t) | (Name _ as t) ->
    dep_typ gen build t
  | Ctype.Const _  | String
  | Result _  | Handle _ | Enum _ | Bitset _ | Bitfields _ -> build
  | FunPtr fn -> dep_fn gen fn build
  | Union fields | Record {fields; _ } ->
    dep_fields gen fields build

let rec generate_ideal dict registry (items,lib as build) p =
    let name = L.make dict p in
    let items = S.remove p items in
    match M.find p registry with
    | Typed.Const c ->
      let lib = add ["const"] name (Const c) lib in
      items,lib
    | Typed.Fn fn ->
      if L.(is_extension dict @@ make dict p) then
        build
      else
      let lib = add ["core"] name (Fn fn) lib in
      items,lib
    | Typed.Type typ ->
      let items, lib = deps (dict,generate_ideal dict registry) build typ in
      let lib = add ["type"] name (Type typ) lib in
      (items,lib)

let rec generate_core dict registry (items, _ as build) =
  if items = S.empty then
    snd build
  else
    let p = S.choose items in
    generate_core dict registry
    @@ generate_ideal dict registry build p
