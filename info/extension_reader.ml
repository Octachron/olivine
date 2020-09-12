open Xml.Infix
open Structured_extensions


let errorf x =
  Fmt.kpf (fun _ -> Fmt.pf Fmt.stderr "@]@."; exit 2) Fmt.stderr ("@[Fatal extension parsing error:@ " ^^ x)


let int_of_string s =
  try int_of_string s with
  | Failure _ -> -1 (**FIXME: constant value *)

let etype n = n%("name")
let command n = n%("name")
let enum n: Structured_extensions.enum =
  let info = match n%?("value") with
    | Some n -> Core (int_of_string n)
    | None ->
      Third_party {
        offset = int_of_string @@ n%("offset");
        upward = not ( n%?("dir") = Some "-");
        extension_number = Option.map int_of_string (n%?("extnumber"))
      } in
  { extend = n%("extends");
    name = n%("name");
    info;
  }
let bit n = { extend = n%("extends");
              pos = int_of_string @@ n%("bitpos");
              name = n%("name") }

let promoted_data x aliases = match x with
  | Xml.Data _ -> errorf "Extension data: unexpected raw data"
  | Node n ->
    match n.name with
    | "enum" when n%??"alias" ->
      N.add (n%"name") (n%"alias") aliases
    | _ -> aliases

let data (type a) x (ext: a data) = match x with
  | Xml.Data _ -> errorf "Extension data: unexpected raw data"
  | Node n ->
    match n.name with
    | "type" -> { ext with types = etype n :: ext.types }
    | "command" -> { ext with commands = command n :: ext.commands }
    | "enum" when n%??"bitpos" ->
      { ext with bits = bit n :: ext.bits }
    | "enum" when n%??"extends"  && (n%??"offset" || n%??"value") ->
      { ext with enums = enum n :: ext.enums }
    | "enum" when n%??"name" -> (* constants *) ext
    | "comment" -> ext
    | _ ->
      errorf "Extension.data: unexpected node %a"
        Xml.pp_xml (Node n)

let extension_requires ext =
  ext
  |> List.map (function
      | Xml.Node { name = "require"; children; _ } -> children
      | _ -> errorf "Non require children to extension nodes"
    )
  |> List.concat

type status =
  | Disabled
  | Active
  | Promoted

let status = function
  | Xml.Data _ -> Active
  | Xml.Node n ->
    if n%?("supported") = Some "disabled" then
      Disabled
    else if
      n%?("promotedto") <> None
    then
      Promoted
    else Active

let ext_metadata n = function
  | Xml.Node ({ name="enum";_} as version) :: Node({name="enum";_} as name)  :: q ->
    let name = name%("name") in
    (* name are now suffixed by "_extension_name" … *)
    let name = String.(sub name 0 (length name - length "_extension_name")) in
    { version = int_of_string (version%("value"));
      name;
      number = int_of_string @@ n%("number");
      type' = n%?("type")
    }, q
  | _ -> errorf "Unexpected structure to extensions children:%a" Xml.pp_xml (Node n)

let feature n q =   n%("number"), q

let extension (type a) (meta: _ -> _ -> a * _) = function
  | Xml.Data _ -> errorf "Extension: unexpected data"
  | Node ({ name = "extension"|"feature"; children; _ } as n) ->
    let all_children = extension_requires children in
    (* TODO: analyze correctly requirements *)
    let metadata, q = meta n all_children in
    let start =
      { metadata; types = []; commands = []; enums = [];
        bits = [] } in
    List.fold_right data q start
  | Node { name; _ } as n ->
    Fmt.epr "@[<hov>%a@]@." Xml.pp_xml n;
    errorf "Extension: unexpected node %s" name


let promoted_extension = function
  | Xml.Data _ -> errorf "Extension: unexpected data"
  | Node ({ name = "extension"|"feature"; children; _ } as n) ->
    let all_children = extension_requires children in
    (* TODO: analyze correctly requirements *)
    let _metadata, q = ext_metadata n all_children in
    List.fold_right promoted_data q N.empty
  | Node { name; _ } as n ->
    Fmt.epr "@[<hov>%a@]@." Xml.pp_xml n;
    errorf "Extension: unexpected node %s" name


let read children =
  let child l x = match status x with
    | Promoted -> Promoted_to (promoted_extension x) :: l
    | Active -> Active (extension ext_metadata x) :: l
    | Disabled -> l in
  List.fold_left child [] children
