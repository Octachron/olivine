module C = Common
module N = C.StringMap
module T = Refined_types

type metadata =
  { name:string;
    number:int;
    type':string option;
    version : int }

type info =
 | Third_party of { offset:int; upward:bool; extension_number: int option }
 | Core of int

type enum =
  { extend:string; name:string; info: info }

type bit = { extend:string; name:string; pos:int}

type 'a data =
  {
    metadata: 'a;
    types: string list;
    commands: string list;
    enums: enum list;
    bits: bit list;
  }


type t = metadata data
type versioned =
  | Active of t
  | Promoted_to of string N.t


let only_active exts = C.List.filter_map
    (function Active x -> Some x | Promoted_to _ -> None) exts


type feature_set = string data

let pp_exttype ppf = function
  | None -> Fmt.pf ppf "support=disabled"
  | Some x -> Fmt.pf ppf "type=\"%s\"" x

let pp_metadata ppf (m:metadata) =
  Fmt.pf ppf
    "@[<hov>{name=%s;@ number=%d;@ %a;@ version=%d}@]"
    m.name m.number pp_exttype m.type' m.version


let pp_enum ppf (e:enum)=
  let pp_info ppf = function
  | Third_party ext ->
    Fmt.pf ppf "Third_party {offset=%d;@ upward=%b}"
      ext.offset ext.upward
  | Core n ->
    Fmt.pf ppf "Core %d" n in
  Fmt.pf ppf
    "@[<hov>{name=%s;@ extend=%s;@ info=%aa}@]"
      e.name e.extend pp_info e.info

let pp_bit ppf (b:bit)=
  Fmt.pf ppf
    "@[<hov>{name=%s;@ extend=%s;@ pos=%d;}@]"
    b.name b.extend b.pos

let pp_active ppf (t:t) =
  Fmt.pf ppf
    "@[<v 2>{metadata=%a;@;types=[%a];@;commands=[%a];@;enums=[%a];@;\
     bits=[%a]@ }@]"
    pp_metadata t.metadata
    Fmt.(list string) t.types
    Fmt.(list string) t.commands
    (Fmt.list pp_enum) t.enums
    (Fmt.list pp_bit) t.bits

let pp ppf = function
  | Active t ->
    pp_active ppf t
  | Promoted_to aliases ->
    Fmt.pf ppf "@[<v>Promoted extension.@,aliases:@[%a@]@]"
      Fmt.(list @@ pair string string) (N.bindings aliases)

module Extend = struct

  module Bound = struct
    type t = {inf:int;sup:int}
    let all = { inf = max_int; sup = min_int }
    let add b x = { sup = max x b.sup; inf  = min x b.inf }

    let extrema =
      List.fold_left add all
  end

 module IntMap = Map.Make(Int)
  let decorate_enum = function
    | Refined_types.Ty.Enum constrs ->
      let add' (b,emap) = function
        | _, T.Abs n as constr ->
          Bound.add b n,
          IntMap.add n constr emap
        | _ -> b, emap in
      List.fold_left add' (Bound.all,IntMap.empty) constrs
    | x -> Fmt.failwith "Expected parent enum, but found: %a" Refined_types.Ty.pp_def x


  let find decorate m0 x m =
    try N.find x m with
    | Not_found ->
      match N.find x m0 with
      | Entity.Type ty -> decorate ty
      | _ -> raise Not_found


  let enum extension_number m0 =
    let find = find decorate_enum m0 in
    let add m (x:enum) =
      let key = x.extend in
      let b, emap = find key m  in
      let abs =
        match x.info with
        | Core n -> n
        | Third_party ext ->
          let extension_number =
            C.Option.merge_exn  ext.extension_number extension_number in
          let pos = (1000000 + extension_number - 1) * 1000 + ext.offset in
          if ext.upward then +pos else -pos in
      let elt = Bound.add b abs, IntMap.add abs (x.name, T.Abs abs) emap in
      N.add key elt m in
    List.fold_left add N.empty

  let bit m0 =
    let proj = function
      | T.Ty.Bitfields x -> x.fields, x.values
      | _ -> raise Not_found in
    let find = find proj m0 in
    let add m (x:bit) =
      let key = x.extend in
      let fields, vals = find key m in
      let l = (x.name, x.pos) :: fields, vals in
      N.add key l m in
    List.fold_left add N.empty

  let extend number m ext =
    let ext_num = number ext in
    let bits = bit m ext.bits in
    let enums = enum ext_num m ext.enums in
    let list emap = List.map snd (IntMap.bindings emap) in
    let rebuild_enum key (_,emap) =
      N.add key (Entity.Type(T.Ty.Enum (list emap))) in
    let rebuild_set key (fields,values) =
      N.add key (Entity.Type(T.Ty.Bitfields { fields; values })) in
    m
    |> N.fold rebuild_enum enums
    |> N.fold rebuild_set bits

  let all_exts m exts =
    let number x = Some x.metadata.number in
    let exts = only_active exts in
    let exts =
      List.sort (fun x y -> compare (number x) (number y)) exts in
    List.fold_left (extend number) m exts

  let update m update =
    let number _x = None in
    List.fold_left (extend number) m update

  let all m upd exts =
    all_exts (update m upd) exts

end
