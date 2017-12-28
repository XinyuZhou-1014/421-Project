%{

open Lambda;;

%}

%token <string> IDENT
%token LAMBDA DOT LPAR RPAR ALPHA BETA EOF

%start exp input
%type <Lambda.lam> exp
%type <Lambda.lam * ((Lambda.tag * Lambda.lam) *((Lambda.tag * Lambda.lam) list))> input

%%

input:
  lhs rhss EOF                    { (snd $1, $2) }

lhs:
  exp                             { (None, $1) }
| rhs                             { (Some (fst $1), snd $1) }

rhss:
  rhs                             { ($1, []) }
| rhs rhss                        { ($1, (fst $2 :: snd $2)) }


rhs:
  ALPHA exp                       { (Alpha, $2) }
| BETA exp                        { (Beta, $2) }

exp:
  no_abs no_app                   { App($1,$2) }
| no_app                          { $1 }

no_abs:
no_abs atom                       { App($1, $2) }
| atom                            { $1 }

no_app:
LAMBDA IDENT DOT exp              { Abs($2, $4) }
| atom                            { $1 }

atom:
| IDENT                           { Var $1 }
| LPAR exp RPAR                   { $2 }
