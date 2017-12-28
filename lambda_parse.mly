%{

open Lambda;;

%}

%token <string> IDENT
%token LAMBDA DOT LPAR RPAR ALPHA EOF

%start exp input
%type <Lambda.lam> exp
%type <Lambda.expr> input

%%

input:
|  exp                             { print_string "!"; One $1 }  
| exp ALPHA exp EOF               { print_string "!!"; Two($1, Alpha, $3) }

exp:
|  no_abs no_app                   { AppLam($1,$2) }
| no_app                          { $1 }

no_abs:
no_abs atom                       { AppLam($1, $2) }
| atom                            { $1 }

no_app:
LAMBDA IDENT DOT exp              { AbsLam($2, $4) }
| atom                            { $1 }

atom:
| IDENT                           { VarLam $1 }
| LPAR exp RPAR                   { $2 }
