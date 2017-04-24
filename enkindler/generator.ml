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
  let pp_constr impl ppf (c, _ ) =
    match impl with
    | Std -> Fmt.pf ppf "%s" c
    | Poly -> Fmt.pf ppf "`%s" c

  let def impl ppf name constrs =
    let constr ppf c = Fmt.pf ppf "|%a" (pp_constr impl) c in
    Fmt.pf ppf "type %s =\n%a\n" name
    (Fmt.list ~sep constr) constrs

  let to_int impl ppf _name constrs =
    Fmt.pf ppf "let to_int = function\n";
    let constr ppf = function
      | (_, Ctype.Abs n as c) -> Fmt.pf ppf "| %a -> %d\n" (pp_constr impl) c n
      | _ -> () in
    Fmt.list ~sep constr ppf constrs

  let of_int impl ppf _name constrs =
    Fmt.pf ppf "let of_int = function\n";
    let constr ppf = function
      | (_, Ctype.Abs n as c) -> Fmt.pf ppf "| %d -> %a\n" n (pp_constr impl) c
      | _ -> () in
    Fmt.list ~sep constr ppf constrs

  let view ppf () =
    Fmt.pf ppf "let view = Ctype.view ~write:to_int ~read:of_int int\n"

  let make impl ppf name constrs =
    Fmt.pf ppf "module %s = struct\n" name;
    def impl ppf name constrs;
    to_int impl ppf name constrs;
    of_int impl ppf name constrs;
    view ppf ();
    Fmt.pf ppf "end\n";
    Fmt.pf ppf "let %s = %s.view \n" name name

end

module Typexp = struct
  let rec pp ppf = function
    | Ctype.Const t -> pp ppf t
    | Ctype.Name n -> Fmt.pf ppf "%s" n
    | Ctype.Ptr typ -> Fmt.pf ppf "(ptr @@ %a )" pp typ
    | Ctype.String -> Fmt.pf ppf "string"
    | Ctype.Array (_,typ) -> Fmt.pf ppf "(ptr @@ %a )" pp typ
    | Ctype.Enum _ | Record _ | Union _ | Bitset _ | Bitfields _ ->
      failwith "Anonymous type"
    | Ctype.Handle _ | Result _ | FunPtr _ ->
      failwith "Not_implemented"
end

module Record = struct

  let field name ppf (field_name,typ)=
    Fmt.pf ppf "let %s = field \"%s\" %a"
      field_name name Typexp.pp typ

  let def ppf name fields =
    Fmt.pf ppf "type t\n";
    Fmt.pf ppf "let t: t structure typ = structure \"%s\"\n"
      name;
    Fmt.list ~sep (field name) ppf fields;
    Fmt.pf ppf "let () = Ctype.seal t\n"

  let make ppf name fields =
    Fmt.pf ppf "module %s = struct\n" name;
    def ppf name fields;
    Fmt.pf ppf "end\n";
    Fmt.pf ppf "let %s = %s.t \n" name
end
