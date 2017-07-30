%token LBRACE
%token RBRACE
%token<string> MACRO
%token<string> WORD
%token LCEIL
%token RCEIL
%token OVER
%token EOF

%left OVER
%nonassoc MACRO


%{ open Latex %}

%start<Latex.t> start
%%

start:
        i=items EOF {i}

items:
  l = list(item) { l }

item:
    | w=WORD {Word w}
    | m=MACRO i=item {Macro(m,[i])}
    | LBRACE i=items RBRACE {Group i}
    | a=item OVER b=item {Macro("over",[a;b])}
    | LCEIL i=items RCEIL {Macro("ceil",[Group i])}
