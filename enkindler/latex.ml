type item =
  | Macro of string * item list
  | Word of string
  | Group of item list
type t = item list
