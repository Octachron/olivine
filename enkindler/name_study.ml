
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

let split_camel_case s =
  let mx = String.length s in
  let sub first after = String.sub s first (after-first) in
  let protect k l start n =
    if n >= mx then
      if start < n then
        List.rev @@ sub start mx :: l
      else l
    else k l start n in
  let rec lower acc start n =
    let c = s.[n] in
    if Char.lowercase_ascii c <> c then
      protect capital (sub start n :: acc) n (n+1)
    else
      protect lower acc start (n+1)
  and capital acc start n =
    let c = s.[n] in
    if Char.uppercase_ascii c = c then
      protect upper acc start (n+1)
    else
      protect lower acc start (n+1)
  and upper acc start n =
    let c = s.[n] in
    if Char.uppercase_ascii c <> c then
      protect lower (sub start n :: acc) n (n+1)
    else
      protect upper acc start (n+1) in
  if s = "" then [] else
    if Char.lowercase_ascii s.[0] = s.[0] then
      protect lower [] 0 1
    else
      protect capital [] 0 1

let clean = function
  | ("",_) :: ("vk",_) :: q -> q
  | ("",_) :: q -> q
  | ("vk",_) :: q -> q
  | p -> p

let lower s = String.lowercase_ascii s, s

let original path =
  let b = Buffer.create 20 in
  List.iter (fun (_, s) -> Buffer.add_string b s) path;
  Buffer.contents b

let path name =
  let path =
  if String.contains name '_' then
    name
    |> String.split_on_char '_'
  else
    name |> split_camel_case
  in
  path |> List.map lower |> clean

let remove_prefix prefix name =
  let rec remove_prefix name prefix current =
    match prefix, current with
    | [] , l -> l
    | (x,_) :: q, (y,_) :: q' when x = y -> remove_prefix name q q'
    | _ :: _, _ -> name in
  remove_prefix name prefix name

let snake ppf () = Fmt.pf ppf "_"

let escape_word (s,_) =
    begin match s.[0] with
      | '0'..'9' -> "n" ^ s
      | _ -> s
    end

let escape = function
  | ["module", _ ] -> [ "module'"]
  | ["type", _  ] -> [ "type'"]
  | ["object", _] -> [ "object'" ]
  | s :: q -> escape_word s :: List.map fst q
  | p -> List.map fst p

let pp_module ppf = function
  | [] -> assert false
  | [a] -> Fmt.pf ppf "%s" ( String.capitalize_ascii @@ escape_word a)
  | a :: q ->
    Fmt.pf ppf "%s_%a" (String.capitalize_ascii @@ escape_word a)
      (Fmt.list ~sep:snake Fmt.string) (List.map fst q)

let pp_constr ppf = pp_module ppf

let pp_type ppf p =
  Fmt.list ~sep:snake Fmt.string ppf (escape p)

let pp_var ppf = pp_type ppf

module N = Misc.StringMap

type nametree =
  | Obj of Typed.entity
  | Node of (int * nametree N.t)

let locate name obj nametree =
  let path = List.map fst @@ path name in
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
  let add_name m (n,_) =
    let count = try 1 + N.find n m with Not_found -> 1 in
    N.add n count m in
  let add_names k _ m =
    List.fold_left add_name m (path k) in
  N.fold add_names e N.empty
