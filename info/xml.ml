type name = { namespace:string; name:string }
module Map = Map.Make(struct type t = name let compare = compare end)
module M = Map

type attribute = { name:name; value:string }
type tag = { name:name; attributes: string M.t }

let fp = Fmt.pf

let pp_name ppf {namespace; name} =
  if namespace = "" then
    fp ppf "%s" name
  else
    fp ppf "%s.%s" namespace name

let pp_attribute ppf (name, value) =
  fp ppf "%a=\"%s\"" pp_name name value

let pp_attributes = Fmt.(iter_bindings M.iter (any " " ++ pp_attribute))

let pp_tag ppf { name; attributes } =
  let attributes = M.bindings attributes in
  fp ppf "<@[<h>%a%s%a@]>" pp_name name (if attributes=[] then "" else " ")
    (Fmt.list pp_attribute) attributes

let pp_tag_end (name,_) ppf = fp ppf "@,</%a>@," pp_name name


type xml =
  | Node of node
  | Data of string
and node = {
  name: string;
  namespace:string;
  attributes: string M.t;
  children: xml list;
  pos: Xmlm.pos;
}

let to_tag ({name; namespace; attributes; _ }: node): tag
    = {name = {name;namespace}; attributes }

let name ?(nms="") name = { namespace = nms; name }

let get n node = M.find (name n) node.attributes

let find n node = match get n node with
  | x -> Some x
  | exception Not_found -> None

module Infix = struct
  let (%) n x = get x n
  let (%?) n x = find x n
  let (%??) n x = M.mem (name x) n.attributes
end

let tree input =
  let name (namespace,name) = { name;namespace} in
  let attribute m (n,value) = M.add (name n) value m in
  let node ( (namespace, name) ,attributes) children  =
    { name; namespace; children; pos = Xmlm.pos input;
      attributes = List.fold_left attribute M.empty attributes } in
  Xmlm.input_doc_tree input
    ~el:(fun t children -> Node (node t children))
    ~data:(fun s -> Data s)

let pp_node_loc ppf node =
  let (lnum, col) = node.pos in
  Fmt.pf ppf "<%s@[<v>%a>@] near %d:%d" node.name pp_attributes node.attributes lnum col

let pp_xml_loc ppf = function
  | Data s -> fp ppf "%S" s
  | Node n  -> pp_node_loc ppf n

let rec pp_xml ppf = function
  | Data s -> fp ppf "\"@,%s@,\"" s
  | Node n  ->
    fp ppf "%a:@;@[<hov 2>%a@]@;" pp_tag (to_tag n)
      (Fmt.list pp_xml) n.children

let normalize tree =
  let rec normalize l =  function
    | Data s -> let s' = String.trim s in
      if s' = "" then l else Data s' :: l
    | Node n ->
      Node { n with children = normalize_fold n.children} :: l
and normalize_fold trees= List.rev @@ List.fold_left normalize [] trees
in
match tree with
| Data _ as d -> d
| Node n  -> Node { n with children = normalize_fold n.children }
