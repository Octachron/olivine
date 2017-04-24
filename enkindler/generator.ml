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
    (Fmt.list ~sep:(fun ppf () -> Fmt.pf ppf "\n") constr) constrs

  let to_int impl ppf _name constrs =
    Fmt.pf ppf "let to_int = function\n";
    let constr ppf = function
      | (_, Ctype.Abs n as c) -> Fmt.pf ppf "| %a -> %d\n" (pp_constr impl) c n
      | _ -> () in
    Fmt.list ~sep:(fun ppf () -> Fmt.pf ppf "\n") constr ppf constrs

  let of_int impl ppf _name constrs =
    Fmt.pf ppf "let of_int = function\n";
    let constr ppf = function
      | (_, Ctype.Abs n as c) -> Fmt.pf ppf "| %d -> %a\n" n (pp_constr impl) c
      | _ -> () in
    Fmt.list ~sep:(fun ppf () -> Fmt.pf ppf "\n") constr ppf constrs

  let view ppf () =
    Fmt.pf ppf "let view = Ctype.view to_int of_int int\n"

  let make_module impl ppf name constrs =
    Fmt.pf ppf "module %s = struct\n" name;
    def impl ppf name constrs;
    to_int impl ppf name constrs;
    of_int impl ppf name constrs;
    view ppf ();
    Fmt.pf ppf "end\n"

end
