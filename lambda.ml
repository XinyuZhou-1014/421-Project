
type tag = Alpha 

type lam = 
  | VarLam of string
  | AbsLam of string * lam
  | AppLam of lam * lam

type expr = One of lam | Two of lam * tag * lam

let rec string_of_lambda lambda tm =
    (match tm with VarLam s -> s
     | AbsLam (s,e) -> lambda^" "^s^". "^(string_of_lambda lambda e)
     | AppLam (rator, rand) ->
       (match rator
        with AbsLam (_,_) -> "("^(string_of_lambda lambda rator)^")"
         | _ -> string_of_lambda lambda rator) ^ " " ^
       (match rand with VarLam s -> s
         | _ ->  "("^(string_of_lambda lambda rand)^")"))
