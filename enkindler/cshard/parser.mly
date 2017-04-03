%token TYPEDEF, CONST
%token STRUCT, ENUM, UNION
%token STAR, EQUAL, PLUS, MINUS, SLASH
%token LPAR,RPAR,LBRACE,RBRACE, LSQ, RSQ
%token<int> INT
%token<Unsigned.UInt.t> UINT
%token<Unsigned.ULLong.t> UINT64
%token<float> FLOAT
%token<string> STRING
%token<string> IDENTIFIER
%token<string> MACRO
%token EOF
%token HASH, BANG, HHASH
%token COLON,SEMI,DOT,COMMA
%token NL
%token LESSER, GREATER, LSHIFT, RSHIFT
%token OR, AND, COMPLEMENT, XOR
%token LN, RN, LT, RT

%left PLUS MINUS
%left LSHIFT RSHIFT
%left OR AND XOR
%nonassoc COMPLEMENT

%{
open Ctype
open Unsigned
%}

%start<unit> start
%start<Ctype.type_decl> typedef
%start<Ctype.field> field
%start <Ctype.num_expr> formula
%%

start:
  EOF {}
;;


typedef:
  TYPEDEF a = def { a }
;;

def:
  | t = typename n = name { n, t }
  | t = typexp LPAR f = fn RPAR args=args
  { f, FunPtr { name=f; return=t; args }   }
;;

raw_typename:
  | t = typename { t }
  | n = IDENTIFIER { Name n }

fn:
  | api = IDENTIFIER STAR n = name { n }
  | STAR n = name { n }
;;

args:
 | LPAR void = IDENTIFIER RPAR { [] }
 | LPAR l = separated_list(COMMA, arg) RPAR { l }
 ;;

arg:
 | t=typexp n=IDENTIFIER { n, t }
;;

field:
 | t=typexp n=name { n, t }
;;


typexp:
  | CONST ty= unconst { Const ty }
  | ty = unconst { ty }
  | STRUCT t = typexp { t }


unconst:
| ty = raw_typename q=qualifier { q ty }

qualifier:
  | {fun x -> x}
  | CONST q = qualifier {fun x -> q(Const x) }
  | STAR q = qualifier { fun x -> Ptr (q x) }
  | LSQ RSQ q = qualifier { fun x -> Array(None, q x) }
  | LSQ n = intexp RSQ q = qualifier { fun x -> Array(Some n, q x)}
;;

intexp:
 | n = INT  { Lit n }
 | ENUM n=name { Var n }

typename:
  LT id = IDENTIFIER RT { Name id }
;;

name:
  LN id = IDENTIFIER RN { id }
;;
formula:
  n=num_expr EOF { n }
;;
num_expr:
  | n = num { n }
  | LPAR n = num_expr RPAR { n }
  | COMPLEMENT n = num_expr {Complement n}
  | n = num_expr MINUS n2=num_expr { Minus (n,n2) }
;;

num:
 | f = FLOAT {Float f}
 | n = INT {Int n}
 | u = UINT {UInt u}
 | u64 = UINT64 {UInt64 u64}
