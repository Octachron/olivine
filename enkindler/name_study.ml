
module Dict = struct
  type t =
    | End of bool
    | Node of { word_end:bool; forest: t array }

  let empty = End false

  let pos = function
    | '0'..'9' as c -> Some( Char.code c - Char.code '0' )
    | 'A'..'Z' as c -> Some( 10 + Char.code c - Char.code 'A' )
    | 'a'..'z' as c -> Some( 36 + Char.code c - Char.code 'a' )
    | _ -> None

  let make_node () = Array.make 62 empty

  let rec add_sub word start stop tree =
    if start >= stop then
      match tree with
      | End _  -> End true
      | Node r -> Node { r with word_end = true }
    else
      match pos word.[start] with
      | None -> tree
      | Some c ->
        match tree with
        | End word_end ->
          let forest = make_node () in
          forest.(c) <- add_sub word (start+1) stop empty;
          Node { word_end; forest }
        | Node a ->
          let t' = add_sub word (start+1) stop a.forest.(c) in
          a.forest.(c) <- t'; Node a

  let add word tree = add_sub word 0 (String.length word) tree

  let read_char char = function
    | End _ -> None
    | Node a ->
      match pos char with
      | None -> None
      | Some p ->
        match a.forest.(p) with
        | End false -> None
        | x -> Some x

  let is_word_end (End t | Node {word_end = t; _ }) = t
  let is_terminal = function
    | End t -> t
    | Node _ -> false

  let rec read_subword w current stop t =
    if current = stop then
      if is_word_end t then
        Some current
      else None
    else if is_terminal t then
      Some current
    else
      match read_char w.[current] t with
      | None -> None
      | Some t -> read_subword w (current+1) stop t

  let read_word_all w start t =
    read_subword w start (String.length w) t

end

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

let is_num = function
  | '0' .. '9' -> true
  | _ -> false

let split_sticky_camel_case dict s =
  let mx = String.length s in
  let sub first after = String.sub s first (after-first) in
  let rec lower acc start n =
    let c = s.[n] in
    if Char.lowercase_ascii c <> c || is_num c then
      capital (sub start n :: acc) n
    else if n + 1 < mx then
      lower acc start (n+1)
    else
      sub start (n+1) :: acc
  and capital acc start =
    let word_stop, n =
    match Dict.read_word_all s start dict with
    | Some stop -> true, stop
    | None -> false, start+1 in
    let stop = n = mx in
    if stop then sub start n :: acc
    else if word_stop then capital ( sub start n :: acc ) n
    else lower acc start n in
  List.rev @@ capital [] 0


type word = string * string option


let lower s =
  let c =   String.lowercase_ascii s in
  c,
  if c = s then None else Some s

let word = lower
let expand = function
  | _, Some x -> x
  | x, None -> x

type role = Prefix | Main | Postfix | Extension

type name =
  { prefix: word list; main: word list; postfix: word list }
let mu = { prefix = []; main = []; postfix = [] }


let rec clean = function
  | ("",_) :: q -> clean q
  | p -> p

let canon w = fst w

let original name =
  let b = Buffer.create 20 in
  let expand path =
    List.iter (fun w -> Buffer.add_string b @@ expand w) path in
  List.iter expand [name.prefix;name.main;List.rev name.postfix];
  Buffer.contents b

module M = Misc.StringMap
type dict = { words:Dict.t; roles: role M.t; context:name }

let compose x y = { prefix = x.prefix @ y.prefix;
                    main = x.main @ y.main;
                    postfix = x.postfix @ y.postfix
                  }

let path dict name =
  let path =
  if String.contains name '_' then
    name
    |> String.split_on_char '_'
  else
    name |> split_sticky_camel_case dict.words
  in
  path |> List.map lower |> clean

let from_path dict path =
  let empty = { prefix = []; main = []; postfix = [] } in
  let rec pre acc = function
    | [] -> acc
    | a :: q as l ->
      begin match M.find (canon a) dict.roles with
        | exception Not_found -> main acc l
        | Prefix -> pre { acc with prefix = a :: acc.prefix } q
        | Main | Postfix | Extension -> main acc l
      end
  and main acc = function
    | [] -> acc
    | a :: q as l ->
      begin match M.find (canon a) dict.roles with
        | exception Not_found -> main { acc with main = a :: acc.main } q
        | Prefix | Main -> main { acc with main = a :: acc.main } q
        | Postfix | Extension -> postfix acc l
      end
  and postfix acc = function
    | [] -> acc
    | a :: q ->
      postfix { acc with postfix = a :: acc.postfix } q in
  let r = pre empty path in
  {r with prefix = List.rev r.prefix; main = List.rev r.main }

let to_path n = n.prefix @ n.main @ n.postfix
let simple path = { mu with main = path}

let remove_prefix prefix name =
  let rec remove_prefix name prefix current =
    match prefix, current with
    | [] , l -> l
    | (x,_) :: q, (y,_) :: q' when x = y -> remove_prefix name q q'
    | _ :: _, _ -> name in
  remove_prefix name prefix name

let remove_context context name =
  { prefix = remove_prefix context.prefix name.prefix;
    main = remove_prefix context.main name.main;
    postfix = remove_prefix context.postfix name.postfix
  }

let make dict string =
  remove_context dict.context @@ from_path dict @@ path dict @@ string
let synthetize dict l =
  l |>  List.map word |> from_path dict

let snake ppf () = Fmt.pf ppf "_"

let escape_word (s,_) =
    begin match s.[0] with
      | '0'..'9' -> "n" ^ s
      | _ -> s
    end

let escape = function
  | ["module", _ ] -> [ "module'" ]
  | ["type", _  ] -> [ "typ" ]
  | ["object", _] -> [ "object'" ]
  | ["false", _ ] -> [ "false'" ]
  | ["true", _ ] -> [ "true'" ]
  | ["inherit",_] -> ["inherit'"]
  | s :: q -> escape_word s :: List.map fst q
  | p -> List.map fst p

let flatten name =
  name.prefix @ name.main @ List.rev name.postfix

let full_pp ppf s =
  let pp_w ppf (s,x) =
    let s = match x with Some s -> s | None -> s in
    Fmt.string ppf s in
  let list = Fmt.list ~sep:snake pp_w in
  Fmt.pf ppf "%a::%a::%a"
    list s.prefix list s.main list s.postfix


let is_extension dict = function
  | { postfix = (a,_) :: _ ; _ } when M.find_opt a dict.roles = Some Extension
    -> true
  | _ -> false

let pp_module ppf n =
  match flatten n with
  | [] -> assert false
  | [a] -> Fmt.pf ppf "%s" ( String.capitalize_ascii @@ escape_word a)
  | a :: q ->
    Fmt.pf ppf "%s_%a" (String.capitalize_ascii @@ escape_word a)
      (Fmt.list ~sep:snake Fmt.string) (List.map fst q)

let pp_constr ppf = pp_module ppf

let pp_type ppf p =
  Fmt.list ~sep:snake Fmt.string ppf (escape @@ flatten p)

let pp_var ppf = pp_type ppf

module N = Misc.StringMap

type nametree =
  | Obj of Typed.entity
  | Node of (int * nametree N.t)

let locate dict name obj nametree =
  let path = List.map fst @@ path dict name in
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

let nametree dict x =
  let m = N.fold (locate dict) x N.empty in
  let c = N.fold (fun _ c s -> s + cardinal c ) m 0 in
  Node(c,m)


let rec pp_nametree ppf = function
  | Obj _ -> ()
  | Node (_,m) ->
    let bs = List.filter (fun (_n,m) -> cardinal m > 5 ) (N.bindings m)
    in
    Fmt.pf ppf "@[<v 2>%a@]"
      (Ctype.Ty.pp_list (Ctype.Ty.const "@;") pp_branch) bs
and pp_branch ppf (name, m) =
    Fmt.pf ppf "%s(%d):@;@[%a@]" name (cardinal m) pp_nametree m


let count_names dict e =
  let add_name m (n,_) =
    let count = try 1 + N.find n m with Not_found -> 1 in
    N.add n count m in
  let add_names k _ m =
    List.fold_left add_name m (path dict k) in
  N.fold add_names e N.empty
