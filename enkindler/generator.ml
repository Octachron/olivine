let sep ppf () = Fmt.pf ppf "\n"
let arrow ppf () = Fmt.pf ppf "@->"

module S = Misc.StringSet
module M = Misc.StringMap

type gen =
  { generator: string -> gen -> gen;
    map: Typed.entity M.t;
    current: S.t }

let remove name g = { g with current = S.remove name g.current }

module Enum = struct

  let contiguous_range =
    let rec range first current = function
      | [] -> Some(min first current, max first current)
      | (_, Ctype.Abs n) :: q when abs(current - n) = 1 ->
        range first n q
      | _ -> None in
    function
    | [] -> None
    | (_, Ctype.Abs n) :: q -> range n n q
    | _ -> None

  type implementation = Std | Poly
  let pp_constr name impl ppf (c, _ ) =
    let name = let open Name_study in
      remove_prefix name (path c) in
    match impl with
    | Std -> Fmt.pf ppf "%a" Name_study.pp_constr name
    | Poly -> Fmt.pf ppf "`%a" Name_study.pp_constr name

  let def impl ppf name constrs =
    let constr ppf c = Fmt.pf ppf "    | %a" (pp_constr name impl) c in
    Fmt.pf ppf "  type t =\n%a" (Fmt.list ~sep constr) constrs

  let to_int impl ppf name constrs =
    Fmt.pf ppf "\n  let to_int = function\n";
    let constr ppf = function
      | (_, Ctype.Abs n as c) ->
        Fmt.pf ppf "    | %a -> %d" (pp_constr name impl) c n
      | _ -> () in
    Fmt.list ~sep constr ppf constrs

  let of_int impl ppf name constrs =
    Fmt.pf ppf "\n  let of_int = function\n";
    let constr ppf = function
      | (_, Ctype.Abs n as c) ->
        Fmt.pf ppf "    | %d -> %a" n (pp_constr name impl) c
      | _ -> () in
    Fmt.list ~sep constr ppf constrs;
    Fmt.pf ppf "\n    | _ -> assert false\n"

  let view ppf () =
    Fmt.pf ppf
       "\n  let view = Ctypes.view ~write:to_int ~read:of_int int\n"

  let make impl ppf name constrs =
    Fmt.pf ppf "module %a = struct\n" Name_study.pp_module name;
    def impl ppf name constrs;
    to_int impl ppf name constrs;
    of_int impl ppf name constrs;
    view ppf ();
    Fmt.pf ppf "end\n";
    Fmt.pf ppf "let %a = %a.view\n\
               type %a = %a.t\n"
      Name_study.pp_var name Name_study.pp_module name
      Name_study.pp_type name Name_study.pp_module name

end

module Typexp = struct
  let rec pp ppf = function
    | Ctype.Const t -> pp ppf t
    | Ctype.Name n ->
      Fmt.pf ppf "%a" Name_study.pp_type (Name_study.path n)
    | Ctype.Ptr (Name n) ->
      Fmt.pf ppf "(ptr %a)" Name_study.pp_type  (Name_study.path n)
    | Ctype.Ptr typ -> Fmt.pf ppf "(ptr (%a))" pp typ
    | Ctype.String -> Fmt.pf ppf "string"
    | Ctype.Array (_,typ) -> Fmt.pf ppf "( ptr (%a) )" pp typ
    | Ctype.Enum _ | Record _ | Union _ | Bitset _ | Bitfields _
    | Ctype.Handle _  ->
      failwith "Anonymous type"
    | Result _ | FunPtr _ ->
      failwith "Not_implemented"
end

module Record = struct

  let rec check_typ  p = function
    | Ctype.Ptr t | Const t -> check_typ p t
    | Array(_,t) -> check_typ p t
    | Name t ->
      if S.mem t p.current then p.generator t p else p
    | _ -> p

  let check_fields = List.fold_left
      (fun acc (_,t) -> check_typ acc t )

  let field name ppf (field_name,typ)=
    let field_name =
      Name_study.(remove_prefix name @@ path field_name) in
    Fmt.pf ppf "  let %a = field t \"%a\" %a"
      Name_study.pp_var field_name Name_study.pp_var field_name
      Typexp.pp typ

  let def ppf name fields =
    Fmt.pf ppf "  type t\n";
    Fmt.pf ppf "  let t: t structure typ = structure \"%a\"\n"
      Name_study.pp_type name;
    Fmt.list ~sep (field name) ppf fields;
    Fmt.pf ppf "\n  let () = Ctypes.seal t\n"

  let make ppf p name fields =
    let p = check_fields p fields in
    Fmt.pf ppf "module %a = struct\n" Name_study.pp_module name;
    def ppf name fields;
    Fmt.pf ppf "end\n";
    Fmt.pf ppf "let %a = %a.t\n\
                type %a = %a.t\n"
      Name_study.pp_type name Name_study.pp_module name
      Name_study.pp_var name Name_study.pp_module name;
    p

end


module Bitset = struct

  let field ppf (name, value) =
    Fmt.pf ppf "  let %a = make_index %d\n"
      Name_study.pp_var (Name_study.path name) value

  let value ppf (name,value) =
    Fmt.pf ppf "  let %a = of_int %d\n"
      Name_study.pp_var (Name_study.path name) value

  let values ppf (fields,values) =
    List.iter (field ppf) fields;
    List.iter (value ppf) values

  let make ppf p name field_name =
    let fields = match M.find field_name p.map with
      | Typed.Type Ctype.Bitfields {fields; values} -> fields, values
      | _ -> [], [] in
    Fmt.pf ppf "module %a = struct\n" Name_study.pp_module name;
    Fmt.pf ppf "  include Bitset.Make()\n";
    values ppf fields;
    Fmt.pf ppf "end\n";
    Fmt.pf ppf "type %a = %a.t\n"
      Name_study.pp_type name Name_study.pp_module name;
    Fmt.pf ppf "let %a = %a.view\n"
      Name_study.pp_var name Name_study.pp_module name;
    p
end

module Handle = struct
  let make ppf name =
    Fmt.pf ppf
      "module %a = Handle.Make()\n\
       type %a = %a.t\n\
       let %a = %a.view\n"
      Name_study.pp_module name
      Name_study.pp_type name Name_study.pp_module name
      Name_study.pp_type name Name_study.pp_module name
end

module Funptr = struct

  let make ppf p tyname (fn:Ctype.fn) =
    let p = Record.check_fields p fn.args in
    let p = Record.check_typ p fn.return in
    let args' = match List.map snd fn.args with
      | [] -> [Ctype.Name "void"]
      | l -> l in
    Fmt.pf ppf "let %a = Foreign.funptr (%a @-> returning %a)\n"
      Name_study.pp_var tyname
      (Fmt.list ~sep:arrow Typexp.pp) args'
      Typexp.pp fn.return;
    p
end

let rec last = function
  | [] -> raise @@ Invalid_argument "last []"
  | [a] -> a
  | _ :: q -> last q

let is_bits name = last name = "bits"

let alias ppf name origin =
  Fmt.pf ppf "let %a = view (fun x -> x) (fun x -> x) %a\n"
    Name_study.pp_var name
    Name_study.pp_type (Name_study.path origin)

let make_type ppf p name = function
  | Ctype.Const _  | Ptr _ | String | Array (_,_)
  | Result _ -> p
  | Name t -> alias ppf name t; p
  | FunPtr fn -> Funptr.make ppf p name fn
  | Union _ ->
    Fmt.(pf stderr) "@{<red> Union not implemented@}@."; p
  | Bitset { field_type; _ } ->
    Bitset.make ppf (remove field_type p) name field_type
  | Bitfields _ -> p (* see Bitset *)
  | Handle _ ->  Handle.make ppf name; p
  | Enum constrs ->
    if not @@ is_bits name then
      Enum.make Enum.Std ppf name constrs
    ; p
  | Record r ->
    Record.make ppf p name r.fields

let right_sys name =
  not @@ List.exists ((=) "android") name

let make_ideal ppf name p =
  let name' = Name_study.path name in
  let p = remove name p in
  if right_sys name' then
    let obj = M.find name p.map in
    match obj with
    | Typed.Type t -> make_type ppf p name' t
    | Fn _f -> p
    | Const _c -> p
  else
    p

let gen ppf map =
  let current = S.of_list @@ List.map fst @@ M.bindings map in
  { generator = make_ideal ppf; current; map}

let make_all ppf map =
  Fmt.pf ppf "open PosixTypes\nopen Ctypes\nopen Foreign\n";
  let g = gen ppf map in
  let rec loop g =
    if g.current = S.empty then
      ()
    else
      loop @@ g.generator (S.choose g.current) g in
  loop g
