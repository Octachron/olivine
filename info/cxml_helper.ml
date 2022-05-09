open Cxml_parser

module Pp = struct
  let fp ppf = Format.fprintf ppf
  let keyword ppf s = fp ppf "@{<keyword>%s@}" s
  let punct ppf s = fp ppf "@{<punctuation>%s@}" s
  let macro ppf s = fp ppf "@{<macro>#%s@}" s
  let op ppf s = fp ppf "@{<op>%s@}" s
  let lit pp ppf = fp ppf "@{<lit>%a@}" pp
  let int ppf = fp ppf "%d"
  let float ppf = fp ppf "%f"
  let string ppf = fp ppf "%s"
  let estring ppf = fp ppf "\"%s\""

type Format.stag +=
  | Keyword
  | Punctuation
  | Macro
  | Lit
  | Op

let mark_open_stag tag =
  let b = "\x1b[1m" in
  match tag with
  | Keyword -> b ^ "\x1b[91m"
  | Punctuation | Macro -> b ^ "\x1b[31m"
  | Lit -> b ^ "\x1b[35m"
  | Op -> b ^ "\x1b[36m"
  | _ -> b

let mark_close_stag _tag =
  "\x1b[0m"
let enable_colors ppf =
  Format.pp_set_tags ppf true;
  Format.pp_set_mark_tags ppf true;
  Format.pp_set_formatter_stag_functions ppf
    { (Format.pp_get_formatter_stag_functions ppf ()) with
      mark_open_stag; mark_close_stag }

end



let pp ppf = let open Pp in
  enable_colors ppf;
  function
  | TYPEDEF -> keyword ppf "typedef"
  | STRUCT -> keyword ppf "struct"
  | ENUM -> keyword ppf "enum"
  | STAR -> op ppf "*"
  | EQUAL -> op ppf "="
  | PLUS -> op ppf "+"
  | MINUS -> op ppf "-"
  | SLASH -> op ppf "/"
  | LPAR -> punct ppf "("
  | RPAR -> punct ppf ")"
  | LSQ -> punct ppf "["
  | RSQ -> punct ppf "]"
  | RBRACE -> punct ppf "}"
  | LBRACE -> punct ppf "{"
  | HASH -> op ppf "#"
  | BANG -> op ppf "!"
  | HHASH -> op ppf "##"
  | INT n -> lit int ppf n
  | UINT n -> lit string ppf (Unsigned.UInt.to_string n)
  | UINT64 n -> lit string ppf (Unsigned.ULLong.to_string n)
  | FLOAT f -> lit float ppf f
  | STRING s -> lit estring ppf s
  | IDENTIFIER s -> fp ppf "%s(id)" s
  | MACRO s -> macro ppf s
  | COLON -> punct ppf ":"
  | DOT -> punct ppf "."
  | SEMI -> punct ppf ";"
  | COMMA -> punct ppf ","
  | NL -> fp ppf "@."
  | OR -> op ppf "|"
  | AND -> op ppf "&"
  | XOR -> op ppf "^"
  | COMPLEMENT -> op ppf "~"
  | LESSER -> op ppf "<"
  | GREATER -> op ppf ">"
  | LSHIFT -> op ppf "<<"
  | RSHIFT -> op ppf ">>"
  | EOF -> fp ppf "EOF"
  | LT -> punct ppf "⦇"
  | RT -> punct ppf "⦈"
  | LN -> punct ppf "⦗"
  | RN -> punct ppf "⦘"
  | CONST -> keyword ppf "const"
  | UNION -> keyword ppf "UNION"

let rec pp_lex ppf lex =
  match Cxml_lexer.start lex with
  | EOF -> ()
  | t -> Pp.fp ppf "%a " pp t; pp_lex ppf lex
