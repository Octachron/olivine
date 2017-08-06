type item =
  | Macro of string * item list
  | Word of string
  | Group of item list
type t = item list

let fp ppf = Format.fprintf ppf

let rec pp_item ppf = function
  | Macro(name,args) -> fp ppf "\%s{%a}" name pp args
  | Word s -> fp ppf "%s" s
  | Group s -> fp ppf "{%a}" pp s
and pp ppf = function
  | [] -> ()
  | [a] -> pp_item ppf a
  | a :: q -> fp ppf "%a,%a" pp_item a pp q

let rec normalize = function
  | Group [x] -> normalize x
  | Macro(n, l) -> Macro(n, List.map normalize l)
  | Group l -> Group (List.map normalize l)
  | Word _ as w -> w
