{ open Latex_parser }

let special = ['\\' '{' '}']
let char = [^ '\\' '{' '}' ' ' '\n' '\t']
let space = [' ' '\t' '\n']

rule start = parse
  | space+ { start lexbuf }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | "\\lceil" { LCEIL }
  | "\\rceil" { RCEIL }
  | "\\over" { OVER }
  | '\\' (char+ as w) { MACRO w }
  | char+ as w { WORD w }
  | eof { EOF }
