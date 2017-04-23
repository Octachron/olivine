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

  let def ppf name constrs =
    let constr ppf (c, _) = Fmt.pf ppf "|%s" c in 
    Fmt.pf ppf "type %s =\n%a\n" name
    (Fmt.list ~sep:(fun ppf () -> Fmt.pf ppf "\n") constr) constrs

  let to_int ppf _name constrs =
    Fmt.pf ppf "let to_int = function\n";
    let constr ppf = function
      | (c, Ctype.Abs n) -> Fmt.pf ppf "| %s -> %d\n" c n
      | _ -> () in
    Fmt.list ~sep:(fun ppf () -> Fmt.pf ppf "\n") constr ppf constrs 

  let of_int ppf _name constrs =
    Fmt.pf ppf "let of_int = function\n";
    let constr ppf = function
      | (c, Ctype.Abs n) -> Fmt.pf ppf "| %d -> %s\n" n c
      | _ -> () in
    Fmt.list ~sep:(fun ppf () -> Fmt.pf ppf "\n") constr ppf constrs 

  let view ppf () =
    Fmt.pf ppf "let view = Ctype.view to_int of_int int\n" 

  let make_module ppf name constrs =
    Fmt.pf ppf "module %s = struct\n" name;
    def ppf name constrs;
    to_int ppf name constrs;
    of_int ppf name constrs;
    view ppf ();
    Fmt.pf ppf "end\n"
  
end
