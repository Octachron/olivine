exception Type_error of string
module N = Common.StringMap
module M = Xml.Map
module T = Retype
module Ty = Retype.Ty
module Arith = Retype.Arith
open Xml.Infix

let debug f = Fmt.epr ("Debug:" ^^ f ^^ "@.")

let type_errorf x =
  Fmt.kpf (fun _ -> Fmt.pf Fmt.stderr "@]@."; exit 2) Fmt.stderr ("@[Fatal type error:@ " ^^ x)

module Option = struct
  let map f = function
    | None -> None
    | Some x -> Some (f x)

  let merge_exn x y = match x, y with
    | Some x, _ -> x
    | _, Some y -> y
    | _ -> assert false
end

type entity =
  | Const of Arith.t
  | Type of Ty.typexpr
  | Fn of Ty.fn

type vendor_id = { name: string; id: int; comment: string }
type short_tag = { name: string; author:string; contact: string}
type c_include = { name: string; system:bool; provide:string option }
type require = { from:string; type_name:string }

module Extension = struct

  type metadata =
    { name:string;
      number:int;
      type':string option;
      version : int }

  type enum =
    { extend:string; name:string; offset:int; upward:bool; extension_number: int option }

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
  type feature_set = string data

  let pp_exttype ppf = function
    | None -> Fmt.pf ppf "support=disabled"
    | Some x -> Fmt.pf ppf "type=\"%s\"" x

  let pp_metadata ppf (m:metadata) =
    Fmt.pf ppf
      "@[<hov>{name=%s;@ number=%d;@ %a;@ version=%d}@]"
      m.name m.number pp_exttype m.type' m.version

  let pp_enum ppf (e:enum)=
    Fmt.pf ppf
      "@[<hov>{name=%s;@ extend=%s;@ offset=%d;@ upward=%b}@]"
      e.name e.extend e.offset e.upward

  let pp_bit ppf (b:bit)=
    Fmt.pf ppf
      "@[<hov>{name=%s;@ extend=%s;@ pos=%d;}@]"
      b.name b.extend b.pos

  let pp ppf (t:t) =
    Fmt.pf ppf
    "@[<v 2>{metadata=%a;@;types=[%a];@;commands=[%a];@;enums=[%a];@;\
     bits=[%a]@ }@]"
      pp_metadata t.metadata
      Fmt.(list string) t.types
      Fmt.(list string) t.commands
      (Fmt.list pp_enum) t.enums
      (Fmt.list pp_bit) t.bits

  module Extend = struct

    type bound = {inf:int;sup:int}
    let all = { inf = max_int; sup = min_int }
    let add b x = { sup = max x b.sup; inf  = min x b.inf }

    let extrema =
      List.fold_left add all

    let decorate_enum = function
      | Ty.Enum constrs ->
        let add' b = function
          | _, T.Abs n -> add b n
          | _ -> b in
        List.fold_left add' all constrs, constrs
      | _ -> raise Not_found


    let find decorate m0 x m =
      try N.find x m with
      | Not_found ->
        match N.find x m0 with
        | Type ty -> decorate ty
        | _ -> raise Not_found

    let enum extension_number m0 =
      let find = find decorate_enum m0 in
      let add m (x:enum) =
        let extension_number =
          Option.merge_exn  x.extension_number extension_number in
        let key = x.extend in
        let b, l = find key m  in
        let pos = (1000000 + extension_number - 1) * 1000 + x.offset in
        let pos = if x.upward then +pos else -pos in
        let elt = add b pos, (x.name, T.Abs pos) ::l in
        N.add key elt m in
      List.fold_left add N.empty

    let bit m0 =
      let proj = function
        | Ty.Bitfields x -> x.fields, x.values
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
      let cmp (_,x) (_,y)= compare x y in
      let sort = List.sort cmp in
      let rebuild_enum key (_,l) =
        N.add key (Type(Ty.Enum (sort l))) in
      let rebuild_set key (fields,values) =
        N.add key (Type(Ty.Bitfields { fields; values })) in
      m
      |> N.fold rebuild_enum enums
      |> N.fold rebuild_set bits

    let all_exts m exts =
      let number x = Some x.metadata.number in
      let exts =
        List.sort (fun x y -> compare (number x) (number y)) exts in
      List.fold_left (extend number) m exts

    let update m update =
      let number _x = None in
      List.fold_left (extend number) m update

    let all m upd exts =
      all_exts (update m upd) exts

  end

end

type spec = {
  vendor_ids: vendor_id list;
  tags: short_tag list;
  entities: entity N.t;
  updates : Extension.feature_set list;
  extensions : Extension.t list;
  includes: c_include list;
  requires: require list;
}



let case cases default x =
  let validate x (attribute, eq) =
    match x%?(attribute) with
    | Some y when eq y -> true
    | Some _ | None -> false in
  let full x (l, _ ) = List.for_all (validate x) l in
  let f l node = (snd @@ List.find (full node) l) node in
  match x with
  | Xml.Data _ -> type_errorf "unexpected data"
  | Node node ->
    try f cases node with Not_found -> default

let flatten tree =
  let b = Buffer.create 100 in
  let rec flatten b = function
    | Xml.Data s -> Buffer.add_string b s
    | Node { name="type"; children; _  } ->
      Buffer.add_string b "⦇";
      List.iter (flatten b) children;
      Buffer.add_string b "⦈"
    | Node { name="name"; children; _  } ->
      Buffer.add_string b "⦗";
      List.iter (flatten b) children;
      Buffer.add_string b "⦘"
    | Node { name="enum"; children; _  } ->
      Buffer.add_string b "enum ⦗";
      List.iter (flatten b) children;
      Buffer.add_string b "⦘"
    | Node { name="comment"; _ } -> ()
    | Node { name; _ } -> type_errorf "Unexpected type node: %s" name
  in
  List.iter (flatten b) tree;
  Buffer.contents b



let ($=) n attr = (n, (=) attr)
let ($=$) n attrs = (n, fun value -> List.exists ((=) value) attrs)
let any _ = true

let tokenize = String.split_on_char ' '


let require_opt = Xml.find "requires"

let is_prefix s s' =
  let exception X in
  try
    String.iteri (fun i x -> if x <> s'.[i] then raise X) s
  ; true
  with X -> false


module Tmp = struct
  open Latex_parser

let fp ppf x = Format.fprintf ppf x

let rec pp ppf =
  function
  | LBRACE -> fp ppf "{"
  | RBRACE -> fp ppf "}"
  | LCEIL -> fp ppf  "⌈"
  | RCEIL -> fp ppf "⌉"
  | WORD s -> fp ppf {|"%s"|} s
  | MACRO(s) -> fp ppf  "\\%s" s
  | OVER -> fp ppf "/"
  | EOF -> fp ppf "eof"
and pp_args ppf x = fp ppf "{%a}" pp x

let rec lexbuf ppf lex =
  match Latex_lexer.start lex with
  | EOF -> pp ppf EOF
  | x -> fp ppf "%a %a" pp x lexbuf lex

end


(* Old version *)
let _len_path s =
     let p = List.filter ((<>) "") @@ String.split_on_char ':' s in
      match p with
      | [] -> assert false
      | p -> Ty.Path p

let len_path s =
  let sub start curr =
    String.sub s start (curr-start) in
  let rec segm prevs start curr =
    if curr >= String.length s then
      if curr > start then
        List.rev (sub start curr :: prevs)
      else
        List.rev prevs
    else
      match s.[curr] with
      | 'a'..'z' | 'A'..'Z' -> segm prevs start (curr+1)
      | '-' ->
        segm ((sub start curr)::prevs) (curr+2) (curr+3)
      | ':' ->
        segm ((sub start curr)::prevs) (curr+1) (curr+2)
      | _ -> assert false in
  Ty.Path (segm [] 0 1)

let len_info s =
  let lens = String.split_on_char ',' s in
  let latex = "latexmath" in
  let len = function
    | "null-terminated" -> Ty.Null_terminated
    | s when is_prefix latex s ->
      let n = String.length s in
      let st = String.length latex  in
      let s = String.sub s (st+2) (n-st-3) in
      debug "latex, %s" s;
      let lex () = Lexing.from_string s in
      debug "tokenized, %a" Tmp.lexbuf (lex ());
      let p = Latex_parser.start Latex_lexer.start (lex ()) in
      debug "parsed, %a" Latex.pp p;
      Ty.Math_expr (Retype.math p)
    | "2*VK_UUID_SIZE" -> (* FIXME *) Const { factor = 2; name = "VK_UUID_SIZE" }
    | s -> len_path s
  in
  List.map len lens

let array_refine node =
  match node%?("len") with
  | None -> fun x -> x
  | Some s ->
    let lens = len_info s in
    let rec refine l q = match l, q with
      | [Ty.Null_terminated] , Ty.(Const Ptr Name "char") ->
        Ty.String
      | len :: l' , Ty.(Ptr x | Const Ptr x) ->
        Ty.Array (Some len, refine l' x)
      | [], ty -> ty
      | _ -> assert false
    in
    refine lens


let rec optionalize l typ = match l, typ with
  | false :: q , Ty.Ptr typ -> Ty.Ptr (optionalize q typ)
  | true :: q, Ptr typ -> Option (Ptr (optionalize q typ))
  | true :: q, Array(n,typ) -> Option (Array (n,optionalize q typ))
  | false :: q, Array(n,typ) -> Array (n,optionalize q typ)
  | q, Const typ -> Const(optionalize q typ)
  | q, Option typ -> Option(optionalize q typ)
  | [true], typ -> Option typ
  | [false], typ -> typ
  | [], typ -> typ
  | _ ->
    Fmt.(pf stderr) "@[optionalize: %a@ [%a]@]@." Ty.pp typ Fmt.(list bool) l;
    raise @@ Invalid_argument "optionalize"

let option_refine node =
  match node%?("optional") with
  | None -> fun x -> x
  | Some l ->
    l
    |> String.split_on_char ','
    |> List.map bool_of_string
    |> optionalize

let structure_refine node t =
  match node%?("validextensionstructs") with
  | None -> t
  | Some l ->
    let l = l |> String.split_on_char ',' |> List.tl in
    Ty.Record_extensions l

let result_refine (s,e) ty =
  let open Ty in
  let sum =String.split_on_char ',' in
  match s, e, ty with
  | None, _, _ | _, None, _  -> ty
  | Some s, Some e, Name "VkResult" ->
    Result { ok = sum s; bad = sum e }
  | _ -> ty

let refine node t =
  structure_refine node @@ option_refine node @@ array_refine node t

let map2 f (x,y) = (x,f y)

let register name entity spec  =
  { spec with entities = N.add name entity spec.entities }

let parse name p s =

  try p Cxml_lexer.start @@ Lexing.from_string s with
    Cxml_parser.Error ->
    Format.eprintf "@[<v> Parsing failure %s :@ %s@]@." name s;
    let lex = Lexing.from_string s in
    Fmt.(pf stderr) "@[<hv> lexing:\n%a\n@.@]"
      Cxml_helper.pp_lex lex;
    exit 2

let typedef spec node =
  let s = flatten node.Xml.children in
  let name, ty =
    map2 (refine node) @@ parse "typedef" Cxml_parser.typedef s in
  register name (Type ty) spec

let is_option_ty = function
  | Ty.Option _ | Const Option _ -> true
  | _ -> false

let fields_refine fields =
  let rec refine extended = function
    | (_, ( Ty.Array(Some Path [index'], _)
          | Option Ty.Array(Some Path [index'],_)) as array )
      :: (name, _ as index) :: q when name = index' ->
      refine (Ty.Array_f { index; array } :: extended) q
    | (n, (Ty.Array(Some Path( a :: _), _) as ty)) :: q ->
      begin match List.assoc a fields |> is_option_ty with
      | exception Not_found -> type_errorf "Not found path component %s" a
      | false -> refine (Ty.Simple(n,ty)::extended) q
      | true -> refine ( Simple(n, Option ty) :: extended) q
      end
    | ("pNext", Ty.Record_extensions exts as ptr) ::
      ("sType", Ty.Name "VkStructureType" as tag) :: q ->
      refine (Ty.Record_extension { tag; ptr; exts } ::extended) q
    | ("pNext", _  as ptr) :: ("sType", _ as tag) :: q ->
      refine (Ty.Record_extension { tag; ptr; exts= [] } ::extended) q
    | ("pNext", ty ) :: q ->
      Fmt.epr "pNext: %a@." Ty.pp ty ;refine extended q
    | ("sType", ty) :: q -> Fmt.epr "sType: %a@." Ty.pp ty ;refine extended q
    | (n,t) :: q -> refine (Ty.Simple(n,t)::extended) q
    | [] -> extended in
  refine [] fields

let discarded_c_helper_structure =
  ["VkBaseInStructure"; "VkBaseOutStructure"]

let structure spec node =
  let name = node%("name") in
  if List.mem name discarded_c_helper_structure then spec else
  let field fields = function
    | Xml.Node ({ name = "member"; children; _ } as n) ->
      let s = flatten children in
      let name, s = parse "field" Cxml_parser.field s in
      (name, refine n s) :: fields
    | _ -> fields in
  let fields = fields_refine @@ List.fold_left field [] node.children in
  let is_private = match node%?("returnedonly") with
    | None -> false | Some b -> bool_of_string b in
  let ty = Ty.Record {fields; is_private} in
  register name (Type ty) spec


let union spec node =
  let name = node%("name") in
  let field fields = function
    | Xml.Node ({ name = "member"; children; _ } as n) ->
      let s = flatten children in
      let name, s = parse "field" Cxml_parser.field s in
      (name, refine n s) :: fields
    | _ -> fields in
  let fields = (List.rev @@ List.fold_left field [] node.children) in
  let ty = Ty.Union fields in
  register name (Type ty) spec

let bitmask spec node =
  match node%?("alias") with
  | Some _ -> spec
  | None ->
    let name, ty = parse "typedef-bitmask" Cxml_parser.typedef
      @@ flatten node.Xml.children in
    let ty =
      match ty with
      | Ty.Name n ->
        Ty.Bitset { implementation=n;
                    field_type = node%?("requires") }
      | _ -> type_errorf "Bitmask expected" in
    register name (Type ty) spec

let handle spec node =
  match node%?("alias") with
  | Some _ -> spec
  | None ->
    let h, name =
      match node.Xml.children with
      | Node h  ::  _ ::
        Node { name ="name"; children = [Data name]; _ } :: _ ->
        h, name
      | _  ->
        type_errorf "Handle type name expected, got %a"
          Xml.pp_xml (Node node)
    in
    let d =
      match h.children with
      | [Data "VK_DEFINE_HANDLE"] -> true
      | [Data "VK_DEFINE_NON_DISPATCHABLE_HANDLE"] -> false
      | _ ->
        type_errorf "Unknown handle type: %a" Xml.pp_xml (Node h) in
    let parent = node%?("parent") in
    let ty = Ty.Handle { dispatchable = d; parent } in
    register name (Type ty) spec

let enum spec node =
  let name = node%("name") in
  if node%??("alias") then spec
  else register name (Type (Ty.Enum [])) spec


let c_include spec node =
  match node.Xml.children with
  | Data l :: Node{ children = [Data name]; _ } :: _ ->
    let system =
      match tokenize l with
      | ["#include";"\""] -> false
      | ["#include";"<"] -> true
      | _ -> assert false in
    let incl = { name; provide=None; system } in
    { spec with includes =  incl :: spec.includes }
  | _ -> spec

let require spec node =
  let r =
    { from = node%("requires");
      type_name = node%("name") }  in
  { spec with requires = r :: spec.requires }


let types spec =
  case [ [ "category" $= "include"], c_include spec;
         [ "category" $= "bitmask"], bitmask spec;
         [ "category" $=$ ["basetype";"funcpointer"]], typedef spec;
         [ "category" $= "struct"], structure spec;
         [ "category" $= "union"], union spec;
         [ "category" $= "handle"], handle spec;
         [ "category" $= "enum"], enum spec;
         [ "requires", any ], require spec;
       ]
    spec

let vendorid = function
  | Xml.Data s -> type_errorf "VendorId: unexpected data %s" s
  | Node  ({children = []; _ } as n) ->
    begin try
        { name = n%("name");
          id = int_of_string @@ n%("id");
          comment = n%("comment")
        } with
      Not_found -> type_errorf "VendorId: wrong field"
    end
  | Node _ -> type_errorf "VendorId: unexpected children"

let short_tag = function
  | Xml.Data s -> type_errorf "Vendor tags: unexpected data %s" s
  | Node ({ children = [];  _ } as n) ->
    let get x = M.find (Xml.name x) n.attributes in
    begin try
        { name = get "name";
          author = get "author";
          contact = get "contact"
        } with
      Not_found -> type_errorf "VendorId: wrong field"
    end
  | Node _ -> type_errorf "VendorId: unexpected children"



let enum_data constrs x =
  match x with
  | Xml.Node ({ name = "enum"; _ } as n) ->
    if n%?("alias") <> None then constrs else
    let pos =
      begin match n%?("value"), n%?("offset") with
        | Some x, _  -> T.Abs (int_of_string x)
        | None, Some x -> T.Offset (int_of_string x)
        | None, None -> assert false
      end in
    (n%("name"), pos) :: constrs
  | Data _ -> constrs
  | Node { name="unused"; _ } -> (*Why?*) constrs
  | Node { name="comment"; _ } -> constrs
  | Node _n as x ->
    type_errorf "Expected enum node, got %a" Xml.pp_xml x

let bitset_data (fields,values) = function
  | Xml.Node ({ name="enum"; _ } as x ) as xml ->
    begin match x%?("alias") with
    | Some _ -> fields, values
    | None ->
      let name = x%("name") in
      begin match x%?("bitpos"), x%?("value") with
        | Some p, None -> (name, int_of_string p) :: fields, values
        | None, Some p -> fields, (name, int_of_string p) :: values
        | _ -> type_errorf "Unknown  bitfield: %a " Xml.pp_xml xml
      end
    end
  | Data s -> type_errorf "Expected bitmask enum, got data@ %s" s
  | Node n -> type_errorf "Expected bitmask enum, got node@ %s" n.name

let constant spec = function
  | Xml.Node ({name="enum"; _ } as n) ->
    if n%?("alias") <> None then spec else
    let name = n%("name") in
    let const = n%("value") in
    let num_expr =
      Arith.simplify @@ parse "formula" Cxml_parser.formula const in
    register name (Const num_expr) spec
  | x -> type_errorf "Unexpected data in constant: %a"
           Xml.pp_xml x


let enums spec x =
  match x%?("name") with
  | Some "API Constants" -> List.fold_left constant spec x.children
  | None -> type_errorf "Unnamed enum"
  | Some n -> match x%?("type") with
    | Some "enum" ->
      let ty = N.find n spec.entities in
      let ty =
        match ty with
        | Type (Ty.Enum constrs) ->
          Ty.Enum (List.fold_left enum_data constrs
                      @@ List.rev x.children)
        | _ -> type_errorf "Enum expected, got %s" n in
      register n (Type ty) spec
    | Some "bitmask" ->
 (*     let n = String.sub n 0 (String.length n - 4) ^ "s" in *)
      begin match N.find_opt n spec.entities with
        | None -> Fmt.(pf stderr) "Not found: %s@." n; spec
        | Some ty ->
        let ty =
        match ty with
        | Type Enum [] ->
          let fields, values =
            List.fold_left bitset_data ([], []) x.children in
          Ty.Bitfields { fields; values}
        | Type ty ->
          type_errorf "Expected bitset %s,@ got@ %a" n Ty.pp ty
        | Fn _ -> type_errorf "Expected a bitset,@ got a function"
        | Const _ -> type_errorf "Expected a bitset,@ got a constant"
      in
      register n (Type ty) spec
      end
    | Some s -> type_errorf "Unknown enum type: %s" s
    | None -> type_errorf "Untyped enum"


let proto n =
  parse "field" Cxml_parser.field @@ flatten n.Xml.children

let arg l = function
  | Xml.Data s -> type_errorf "expected function arg, got data: %s" s
  | Node {name="implicitexternsyncparams"; _ } ->
    (* TODO *) l
  | Node ({ name = "param"; _ } as n) ->
    (map2 (refine n) @@ parse "field" Cxml_parser.field
     @@ flatten n.children) :: l
  | Node n ->
    type_errorf "expected param node, got %s node " n.name

let args_refine _name args =
  let rec refine = function
    | [] -> [](*
    | [Ty.Simple (_,Ty.Ptr _) as field] -> [Ty.{field; dir = Out }]
    | [Ty.Simple (_,Ty.Array(Some(Path _), Name _ )) as field] ->
      [Ty.{field; dir = Out }] (*Not enough information here*) *)
    | Ty.Array_f { index=(_,Ty.Ptr _); _ } as field :: q ->
      { Ty.field; dir = Out } :: refine q
    | field :: q -> { field; dir = In_Out} :: refine q in
  refine @@ fields_refine args

let command spec = function
  | Xml.Data s -> type_errorf "expected command node, got data %s" s
  | Node ( { name="command";
             children = Node ({ name = "proto"; _ } as p) :: args;
             _ } as n) ->
    let r = n%?("successcodes"), n%?("errorcodes") in
    let name, return = proto p in
    let return = result_refine r return in
    register name
      (Fn { Ty.return; name; original_name=name;
            args = args_refine name @@ List.fold_left arg [] args })
      spec
  | Node n as x-> if n%?("alias") <> None then spec else
      type_errorf "expected command node, got %s node: %a"
        n.name Xml.pp_xml x

module Extension_reader = struct

  let int_of_string s =
    try int_of_string s with
    | Failure _ -> -1 (**FIXME: constant value *)

  let etype n = n%("name")
  let command n = n%("name")
  let enum n: Extension.enum =
    { Extension.extend = n%("extends");
      name = n%("name");
      offset = int_of_string @@ n%("offset");
      upward = not ( n%?("dir") = Some "-");
      extension_number = Option.map int_of_string (n%?("extnumber"))
    }
  let bit n = { Extension.extend = n%("extends");
                pos = int_of_string @@ n%("bitpos");
                name = n%("name") }

  let data (type a) x (ext: a Extension.data) = match x with
    | Xml.Data _ -> type_errorf "Extension data: unexpected raw data"
    | Node n ->
      match n.name with
      | "type" -> { ext with types = etype n :: ext.types }
      | "command" -> { ext with commands = command n :: ext.commands }
      | "enum" when n%??"bitpos" ->
        { ext with bits = bit n :: ext.bits }
      | "enum" when n%??"offset" ->
        { ext with enums = enum n :: ext.enums }
      | "enum" when n%??"value" || n%??"name" -> (*FIXME*) ext
      | "comment" -> ext
      | _ ->
        type_errorf "Extension.data: unexpected node %a"
             Xml.pp_xml (Node n)

  let extension_requires ext =
    ext
    |> List.map (function
        | Xml.Node { name = "require"; children; _ } -> children
        | _ -> type_errorf "Non require children to extension nodes"
      )
    |> List.concat


  let enabled = function
    | Xml.Data _ -> true
    | Xml.Node n ->
      not (n%?("supported") = Some "disabled")
      && not ( n%?("promotedto") <> None)

let ext_metadata n = function
  | Xml.Node ({ name="enum";_} as version) :: Node({name="enum";_} as name)  :: q ->
    let name = name%("name") in
    (* name are now suffixed by "_extension_name" … *)
    let name = String.(sub name 0 (length name - length "_extension_name")) in
    { Extension.version = int_of_string (version%("value"));
      name;
      number = int_of_string @@ n%("number");
      type' = n%?("type")
    }, q
  | _ -> type_errorf "Unexpected structure to extensions children:%a" Xml.pp_xml (Node n)

let feature n q =   n%("number"), q

let extension (type a) (meta: _ -> _ -> a * _) = function
  | Xml.Data _ -> type_errorf "Extension: unexpected data"
  | Node ({ name = "extension"|"feature"; children; _ } as n) ->
    let all_children = extension_requires children in
    (* TODO: analyze correctly requirements *)
    let metadata, q = meta n all_children in
        let start =
          { Extension.metadata; types = []; commands = []; enums = [];
            bits = [] } in
        List.fold_right data q start
  | Node { name; _ } as n ->
    Fmt.epr "@[<hov>%a@]@." Xml.pp_xml n;
    type_errorf "Extension: unexpected node %s" name
end

let extend spec =
  { spec with
    entities = Extension.Extend.all spec.entities
        spec.updates
        spec.extensions }
let add_extension spec children =
  { spec with
    extensions =
      spec.extensions @
      List.map Extension_reader.(extension ext_metadata)
      @@ List.filter Extension_reader.enabled
        children }

let add_feature spec children =
  { spec with
    updates =
      spec.updates @
      List.map Extension_reader.(extension feature)
        children }

let section spec x =
  match x with
  | Xml.Data _ -> spec
  | Node ({name;children; _ } as n) ->
    match name with
    | "tags" -> { spec with tags =
                              List.map short_tag children @ spec.tags }
    | "vendorids" ->
      { spec with vendor_ids=
                    List.map vendorid children @ spec.vendor_ids }
    | "types" -> List.fold_left types spec children
    | "enums" ->
      enums spec n
    | "commands" ->
      List.fold_left command spec n.children
    | "extensions"  -> add_extension spec n.children
    | "feature" -> add_feature spec [Node n]
    | _ -> spec


let typecheck tree =
  let root spec = function
    | Xml.Node { children; _ } ->
      List.fold_left section spec children
    | Data _ -> type_errorf "root: unexpected data"
  in
  extend @@ root {
    vendor_ids = [];
    tags = [];
    entities = N.empty;
    updates = [];
    includes = [];
    requires = [];
    extensions = [];
  } tree

let fp = Fmt.pf

let pp_vendorid ppf (v:vendor_id) =
  fp ppf "@[<hov>{name=%s;@ id=%d;@ comment=%s}@]" v.name v.id v.comment

let pp_short_tag ppf (v:short_tag) =
  fp ppf "@[<hov>{name=%s;@ author=%s;@ contact=%s}@]"
    v.name v.author v.contact

let pp_include ppf (incl:c_include) =
  fp ppf "include {name:%s; system:%b}" incl.name incl.system

let pp_required ppf r =
  fp ppf "require {type_name:%s; from:%s}"
    r.type_name r.from

let pp_entity ppf (name,ent)= match ent with
  | Fn fn -> fp ppf "%a@;" Ty.pp_fn fn
  | Type ty -> fp ppf "%a@;" Ty.pp_typedecl (name,ty)
  | Const c -> fp ppf "constant %s=%a@;" name Arith.pp c

let pp_constant ppf (name, expr) =
  Fmt.pf ppf "%s=%a" name Arith.pp expr

let pp ppf r =
  fp ppf "@[<v 2>{@,vendor ids=@ @[<v>%a@];@;\
          tags =@;@[<v>%a@] @;\
          includes=@;[@[<v>%a@]@,]@;\
          requires=@;[@[<v>%a@]@,]@;\
          entities =@ [@[<v>%a@]]@;\
          extensions=@ @[<hov 2>%a@]@;\
          }@]@."
    (Fmt.list pp_vendorid) r.vendor_ids (Fmt.list pp_short_tag) r.tags
    (Fmt.list pp_include) r.includes
    (Fmt.list pp_required) r.requires
    Fmt.(list @@ pp_entity ) (N.bindings r.entities)
    (Fmt.list Extension.pp) r.extensions
