let sep ppf () = Fmt.pf ppf "\n"

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
    Fmt.list ~sep constr ppf constrs

  let view ppf () =
    Fmt.pf ppf
      "\n  let view = Ctype.view ~write:to_int ~read:of_int int\n"

  let make impl ppf name constrs =
    let name = Name_study.path name in
    Fmt.pf ppf "module %a = struct\n" Name_study.pp_module name;
    def impl ppf name constrs;
    to_int impl ppf name constrs;
    of_int impl ppf name constrs;
    view ppf ();
    Fmt.pf ppf "end\n";
    Fmt.pf ppf "let %a = %a.view\n"
      Name_study.pp_var name Name_study.pp_module name

end

module Typexp = struct
  let rec pp ppf = function
    | Ctype.Const t -> pp ppf t
    | Ctype.Name n -> Fmt.pf ppf "%s" n
    | Ctype.Ptr typ -> Fmt.pf ppf "(ptr @@ %a)" pp typ
    | Ctype.String -> Fmt.pf ppf "string"
    | Ctype.Array (_,typ) -> Fmt.pf ppf "( ptr @@ %a )" pp typ
    | Ctype.Enum _ | Record _ | Union _ | Bitset _ | Bitfields _ ->
      failwith "Anonymous type"
    | Ctype.Handle _ | Result _ | FunPtr _ ->
      failwith "Not_implemented"
end

module Record = struct

  let field name ppf (field_name,typ)=
    let field_name =
      Name_study.(remove_prefix name @@ path field_name) in
    Fmt.pf ppf "  let %a = field \"%a\" %a"
      Name_study.pp_var field_name Name_study.pp_type name
      Typexp.pp typ

  let def ppf name fields =
    Fmt.pf ppf "  type t\n";
    Fmt.pf ppf "  let t: t structure typ = structure \"%a\"\n"
      Name_study.pp_type name;
    Fmt.list ~sep (field name) ppf fields;
    Fmt.pf ppf "\n  let () = Ctype.seal t\n"

  let make ppf name fields =
    let name = Name_study.path name in
    Fmt.pf ppf "module %a = struct\n" Name_study.pp_module name;
    def ppf name fields;
    Fmt.pf ppf "end\n";
    Fmt.pf ppf "let %a = %a.t\n"
      Name_study.pp_var name Name_study.pp_module name

end


module Bitset = struct
  let make ppf name =
    let name = Name_study.path name in
    Fmt.pf ppf "module %a = Bitset.Make()\n" Name_study.pp_module name
end

module Handle = struct
  let make ppf name =
    let name = Name_study.path name in
    Fmt.pf ppf "module %a = Handle.Make()\n" Name_study.pp_module name
end



let make_type ppf name = function
  | Ctype.Const _ | Name _ | Ptr _ | String | Array (_,_)
  | Result _ -> ()

  | FunPtr _ -> Fmt.(pf stderr) "@{<red> FunPtr not implemented@}@."
  | Union _ -> Fmt.(pf stderr) "@{<red> Union not implemented@}@."
  | Bitset _ -> Bitset.make ppf name
  | Bitfields _ -> Fmt.(pf stderr)
                     "@{<red> Bitfields not implemented@}@."
  | Handle _ ->  Handle.make ppf name
  | Enum constrs ->
    Enum.make Enum.Std ppf name constrs
  | Record r ->
    Record.make ppf name r.fields

let make ppf (name,obj)=
  match obj with
  | Typed.Type t -> make_type ppf name t
  | Fn _f -> ()
  | Const _c -> ()
