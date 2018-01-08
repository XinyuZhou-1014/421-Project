%{

open Lambda;;

%}

%token <string> IDENT
%token LAMBDA DOT LPAR RPAR ALPHA EOF DSEMI

%start input
%type <Lambda.lam> exp
%type <Lambda.expr> input

%%

input:
| exp_no_eof                      { $1 }
| exp_no_eof DSEMI                { $1 }

exp_no_eof: 
| exp                             { One $1 }  
| exp ALPHA exp                   { Two($1, $3) }

exp:
| no_abs no_app                   { AppLam($1,$2) }
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
