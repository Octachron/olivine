exception Type_error of string
module N = Misc.StringMap
module M = Xml.Map
open Xml.Infix

type entity =
  | Const of Ctype.num_expr
  | Type of Ctype.typexpr
  | Fn of Ctype.fn

type vendor_id = { name: string; id: int; comment: string }
type short_tag = { name: string; author:string; contact: string}
type c_include = { name: string; system:bool; provide:string option }
type require = { from:string; type_name:string }

type spec = {
  vendor_ids: vendor_id list;
  tags: short_tag list;
  entities: entity N.t;
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
  | Xml.Data _ -> raise @@ Type_error "unexpected data"
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

    | Node { name; _ } -> raise @@ Type_error ("Unexpected type node: " ^ name)
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

let len_info s =
  let lens = String.split_on_char ',' s in
  let len = function
    | "null-terminated" -> Ctype.Null_terminated
    | s when is_prefix "latexmath" s -> Ctype.Math_expr
    | s -> Ctype.Var s in
  List.map len lens

let array_refine node =
  match node%?("len") with
  | None -> fun x -> x
  | Some s ->
    let lens = len_info s in
    let rec refine l q = match l, q with
      | [Ctype.Null_terminated] , Ctype.(Const Ptr Name "char")
        -> Ctype.String
      | len :: l' , Ctype.(Ptr x | Const Ptr x) ->
        Ctype.Array (Some len, refine l' x)
      | [], ty -> ty
      | _ -> assert false
    in
    refine lens

let result_refine (s,e) ty =
  let open Ctype in
  let sum =String.split_on_char ',' in
  match s, e, ty with
  | None, _, _ | _, None, _  -> ty
  | Some s, Some e, Name "VkResult" -> Result { ok = sum s; bad = sum e }
  | _ -> ty


let refine node t =
  array_refine node t

let map2 f (x,y) = (x,f y)

let register name entity spec  =
  { spec with entities = N.add name entity spec.entities }

let parse p s =
(*  let lex = Lexing.from_string s in
  Fmt.(pf stderr) "lexing:\n%a\n%!"
    Cp__helper.pp_lex lex; *)
  p Lexer.start @@ Lexing.from_string s

let typedef spec node =
  let s = flatten node.Xml.children in
  let name, ty = map2 (refine node) @@ parse Parser.typedef s in
  register name (Type ty) spec

let structure spec node =
  let name = node%("name") in
  let field fields = function
    | Xml.Node ({ name = "member"; children; _ } as n) ->
      let s = flatten children in
      let name, s = parse Parser.field s in
      (name, refine n s) :: fields
    | _ -> fields in
  let fields = (List.rev @@ List.fold_left field [] node.children) in
  let is_private = match node%?("returnedonly") with
    | None -> false | Some b -> bool_of_string b in
  let ty = Ctype.Record {fields; is_private} in
  register name (Type ty) spec

let bitmask spec node =
  let name, ty = parse Parser.typedef @@ flatten node.Xml.children in
  let ty =
    match ty with
    | Ctype.Name n ->
      Ctype.Bitset { implementation=n; field_type = node%("requires") }
    | _ -> raise @@ Type_error "Bitmask expected" in
  register name (Type ty) spec

let handle spec node =
  let h, name =
    match node.Xml.children with
    | Node h  ::  _ ::
      Node { name ="name"; children = [Data name]; _ } :: _ ->
      h, name
    | _  -> raise @@ Type_error "Handle type name expected" in
  let d =
    match h.children with
    | [Data "VK_DEFINE_HANDLE"] -> true
    | [Data "VK_DEFINE_NON_DISPATCHABLE_HANDLE"] -> false
    | _ -> raise @@ Type_error "Unknown handle type" in
  let parent = node%?("parent") in
  let ty = Ctype.Handle { dispatchable = d; parent } in
  register name (Type ty) spec

let enum spec node =
  let name = node%("name") in
  register name (Type (Ctype.Enum [])) spec


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
         [ "category" $= "handle"], handle spec;
         [ "category" $= "enum"], enum spec;
         [ "requires", any ], require spec;
       ]
    spec


let vendorid = function
  | Xml.Data _ -> raise @@ Type_error "VendorId: unexpected data"
  | Node  ({children = []; _ } as n) ->
    begin try
        { name = n%("name");
          id = int_of_string @@ n%("id");
          comment = n%("comment")
        } with
      Not_found -> raise @@ Type_error "VendorId: wrong field"
    end
  | Node _ -> raise @@ Type_error "VendorId: unexpected children"

let short_tag = function
  | Xml.Data _ -> raise @@ Type_error "Vendor tags: unexpected data"
  | Node ({ children = [];  _ } as n) ->
    let get x = M.find (Xml.name x) n.attributes in
    begin try
        { name = get "name";
          author = get "author";
          contact = get "contact"
        } with
      Not_found -> raise @@ Type_error "VendorId: wrong field"
    end
  | Node _ -> raise @@ Type_error "VendorId: unexpected children"



let enum_data constrs x =
  match x with
  | Xml.Node ({ name = "enum"; _ } as n) ->
    let pos =
      begin match n%?("value"), n%?("offset") with
        | Some x, _  -> Ctype.Abs (int_of_string x)
        | None, Some x -> Ctype.Offset (int_of_string x)
        | None, None -> assert false
      end in
    (n%("name"), pos) :: constrs
  | Data _ -> constrs
  | Node { name="unused"; _ } -> (*Why?*) constrs
  | Node n -> raise @@ Type_error
      ("Expected enum node, got " ^ n.name ^ " node")
let bitset_data (fields,values) = function
  | Xml.Node ({ name="enum"; _ } as x ) ->
    let name = x%("name") in
    begin match x%?("bitpos"), x%?("value") with
      | Some p, None -> (name, int_of_string p) :: fields, values
      | None, Some p -> fields, (name, int_of_string p) :: values
      | _ -> assert false
    end
  | Data s -> raise @@ Type_error ("Expected bitmask enum, got data " ^ s)
  | Node n -> raise @@ Type_error ("Expected bitmask enum, got node " ^ n.name)
let constant spec = function
  | Xml.Node ({name="enum"; _ } as n) ->
    let name = n%("name") in
    let const = n%("value") in
    let num_expr = Ctype.simplify @@ parse Parser.formula const in
    register name (Const num_expr) spec
  | _ -> raise @@ Type_error "Unexpected data in constant"


let enums spec x =
  match x%?("name") with
  | Some "API Constants" -> List.fold_left constant spec x.children
  | None -> raise @@ Type_error "Unnamed enum"
  | Some n -> match x%?("type") with
    | Some "enum" ->
      let ty = N.find n spec.entities in
      let ty =
        match ty with
        | Type (Ctype.Enum constrs) ->
          Ctype.Enum (List.fold_left enum_data constrs
                      @@ List.rev x.children)
        | _ -> raise @@ Type_error ("Enum expected, got " ^ n) in
      register n (Type ty) spec
    | Some "bitmask" ->
      let ty = N.find n spec.entities in
      let ty =
        match ty with
        | Type Enum [] ->
          let fields, values =
            List.fold_left bitset_data ([], []) x.children in
          Ctype.Bitfields { fields; values}
        | Type ty -> raise @@ Type_error
            (Format.asprintf "Expected bitset %s, got %a" n Ctype.pp ty)
        | Fn _ -> raise @@ Type_error "Expected a bitset, got a function"
        | Const _ -> raise @@ Type_error "Expected a bitset, got a constant"
      in

      register n (Type ty) spec
    | Some s -> raise @@ Type_error ("Unknown enum type: " ^ s)
    | None -> raise @@ Type_error "Untyped enum"


let proto n =
  parse Parser.field @@ flatten n.Xml.children

let arg l = function
  | Xml.Data s -> raise @@ Type_error ("expected function arg, got data: " ^ s )
  | Node {name="implicitexternsyncparams"; _ } ->
    (* TODO *) l
  | Node ({ name = "param"; _ } as n) ->
    (map2 (refine n) @@ parse Parser.field @@ flatten n.children) :: l
  | Node n -> raise @@ Type_error ("expected param node, got "^ n.name ^ " node")


let command spec = function
  | Xml.Data s -> raise @@ Type_error ("expected command node, got data"^ s)
  | Node ( { name="command";
             children = Node ({ name = "proto"; _ } as p) :: args; _ } as n) ->
    let r = n%?("successcodes"), n%?("errorcodes") in
    let name, return = proto p in
    let return = result_refine r return in
    register name
      (Fn { Ctype.return; name; args = List.rev @@ List.fold_left arg [] args })
      spec
  | Node n -> raise @@ Type_error ("expected command node, got "^ n.name ^ " node")

let section spec x =
  match x with
  | Xml.Data _ -> spec
  | Node ({name;children; _ } as n) ->
    match name with
    | "tags" -> { spec with tags = List.map short_tag children @ spec.tags }
    | "vendorids" ->
      { spec with vendor_ids= List.map vendorid children @ spec.vendor_ids }
    | "types" -> List.fold_left types spec children
    | "enums" ->
      enums spec n
    | "commands" ->
      List.fold_left command spec n.children
    | _ -> spec

let typecheck tree =
  let root spec = function
    | Xml.Node { children; _ } ->
      List.fold_left section spec children
    | Data _ -> raise @@ Type_error "root: unexpected data"
  in
  root {
    vendor_ids = [];
    tags = [];
    entities = N.empty;
    includes = [];
    requires = [];
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
  | Fn fn -> fp ppf "%a@;" Ctype.pp_fn fn
  | Type ty -> fp ppf "%a@;" Ctype.pp_typedecl (name,ty)
  | Const c -> fp ppf "constant %s=%a@;" name Ctype.pp_num_expr c

let pp_constant ppf (name, expr) =
  Fmt.pf ppf "%s=%a" name Ctype.pp_num_expr expr

let pp ppf r =
  fp ppf "@[<v 2>{@,vendor ids=@ @[<v>%a@];@;\
          tags =@;@[<v>%a@] @;\
          includes=@;[@[<v>%a@]@,]@;\
          requires=@;[@[<v>%a@]@,]@;\
          entities =@ [@[<v>%a@]]@;\
          }@]@."
    (Fmt.list pp_vendorid) r.vendor_ids (Fmt.list pp_short_tag) r.tags
    (Fmt.list pp_include) r.includes
    (Fmt.list pp_required) r.requires
    Fmt.(list @@ pp_entity ) (N.bindings r.entities)
