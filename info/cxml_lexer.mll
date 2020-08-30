{
open Cxml_parser
let string_buffer = Buffer.create 50
}

let digit = ['0'-'9']
let space = [' ' '\t']
let idstart = ['a'-'z''A'-'Z''_']
let idmid = idstart|digit|'_'
let newline = ('\013'* '\010')


rule start = parse
 | "#" (idstart idmid* as m) { MACRO m }
 | "struct"  { STRUCT }
 | "typedef" { TYPEDEF }
 | "union" { UNION }
 | "enum" { ENUM }
 | "const" { CONST }
 | "⦇"  {LT}
 | "⦈"  {RT}
 | "⦗"  {LN}
 | "⦘"  {RN}
 | "=" { EQUAL }
 | "*" { STAR }
 | "(" { LPAR }
 | ")" { RPAR }
 | '[' { LSQ }
 | ']' { RSQ }
 | "{" { LBRACE }
 | "}" { RBRACE }
 | "," { COMMA }
 | ":" { COLON }
 | ";" { start lexbuf }
 | "+" { PLUS }
 | "-" { MINUS }
 | "." { DOT }
 | "|" { OR }
 | "&" { AND }
 | "/" { SLASH }
 | "<" {LESSER}
 | "<<" {LSHIFT}
 | ">" {GREATER}
 | ">>" {RSHIFT}
 | "~" {COMPLEMENT}
 | "^" {XOR}
 | "//" { comment lexbuf }
 | digit+ as d 'L'?{ INT(int_of_string d)  }
 | digit+ as d 'U' 'L'? { UINT( Unsigned.UInt.of_string d)  }
 | digit+ as d 'U' 'L' 'L' { UINT64( Unsigned.ULLong.of_string d)  }
 | digit+ as d "f"  { FLOAT(float @@ int_of_string d) }
 | digit+ "." digit* as f "f"? { FLOAT(float_of_string f) }
 | space+ {start lexbuf}
 | newline+  { start lexbuf }
 | idstart idmid* as d { IDENTIFIER d }
 | "\"" {string lexbuf}
 | "##" { HHASH  }
 | "#" { HASH }
 | "!" { BANG }
 | "\\\n" {start lexbuf}
 | "/*" { longcomment lexbuf }
 | eof {EOF}


and comment = parse
| "\n" {start lexbuf}
| _ {comment lexbuf}

and longcomment = parse
| "*/" {start lexbuf}
| _ {longcomment lexbuf}

and string = parse
| "\"" { let s = Buffer.contents string_buffer in Buffer.reset string_buffer;
         STRING s }
| _ as c { Buffer.add_char string_buffer c; string lexbuf }
