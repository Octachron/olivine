let split_on_pred pred s =
  let len = String.length s in
  let rec analyze start curr =
    if curr >= len then
      [String.sub s start (curr - start)]
    else if pred s.[curr] then
        String.sub s start (curr - start) :: analyze curr (curr + 1)
    else
      analyze start (curr + 1) in
  analyze 0 0

let path name =
  if String.contains name '_' then
    name |> String.split_on_char '_' |> List.map String.lowercase_ascii
  else
    name |> split_on_pred (fun x -> Char.lowercase_ascii x <> x)
    |> List.map String.uncapitalize_ascii

let remove_prefix prefix name =
  let rec remove_prefix name prefix current =
    match prefix, current with
    | [] , l -> l
    | x :: q, y :: q' when x = y -> remove_prefix name q q'
    | x :: _, _ -> name in
  remove_prefix name prefix name

let snake ppf () = Fmt.pf ppf "_"

let pp_module ppf = function
  | [] -> assert false
  | a :: q ->
    Fmt.pf ppf "%s_%a" (String.capitalize_ascii a)
      (Fmt.list ~sep:snake Fmt.string) q

let pp_constr ppf = pp_module ppf

let pp_type ppf =
  Fmt.list ~sep:snake Fmt.string ppf

module N = Misc.StringMap

type nametree =
  | Obj of Typed.entity
  | Node of (int * nametree N.t)

let locate name obj nametree =
  let path = path name in
  let rec locate nametree = function
    | [] -> assert false
    | [a] -> N.add a (Obj obj) nametree
    | a :: (_ :: _  as q) ->
        let subtree =
          match N.find a nametree with
          | sm -> sm
          | exception Not_found -> Node(0, N.empty) in
        let n, sm =
          match subtree with
          | Obj _ -> 0, N.empty
          | Node (n, sm)  -> n, sm in
            N.add a (Node(n + 1, locate sm q)) nametree
  in
  locate nametree path


let cardinal = function
  | Obj _ -> 1
  | Node (n, _ ) -> n

let nametree x =
  let m = N.fold locate x N.empty in
  let c = N.fold (fun _ c s -> s + cardinal c ) m 0 in
  Node(c,m)


let rec pp_nametree ppf = function
  | Obj _ -> ()
  | Node (_,m) ->
    let bs = List.filter (fun (_n,m) -> cardinal m > 5 ) (N.bindings m)
    in
    Fmt.pf ppf "@[<v 2>%a@]"
      (Ctype.pp_list (Ctype.const "@;") pp_branch) bs
and pp_branch ppf (name, m) =
    Fmt.pf ppf "%s(%d):@;@[%a@]" name (cardinal m) pp_nametree m


let count_names e =
  let add_name m n =
    let count = try 1 + N.find n m with Not_found -> 1 in
    N.add n count m in
  let add_names k _ m =
    List.fold_left add_name m (path k) in
  N.fold add_names e N.empty
