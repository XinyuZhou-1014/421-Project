{
open Lambda_parse;;
}

let numeric = ['0' - '9']
let alpha = ['a' - 'z' 'A' - 'Z' ]
let letter =['a' - 'z' 'A' - 'Z' '_']

let id_char = numeric | letter | "'"
let whitespace = [' ' '\t' '\n']

rule token = parse
  | whitespace      { token lexbuf }  (* skip over whitespace *)
  | eof             { EOF }
  | "%"             { LAMBDA }
  | "."             { DOT }
  | "("             { LPAR }
  | ")"             { RPAR }
  |  (alpha (id_char*)) as s     { IDENT s }
  | "~a~"           { ALPHA }
{
}
