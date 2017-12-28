(* File: lambdaTest.ml *)
(* Author: Elsa L. Gunter *)
(* Copyright: 2016 *)
(* Share and Enjoy *)

open Lambda
open Lambda_parse

let rec loop () =
    (print_endline "> ";
     let tm = Lambda_parse.input Lambda_lex.token (Lexing.from_channel stdin) in
     match tm with 
     | Two (tm1, tm2) ->
     print_endline (string_of_lambda "%" tm1);
     print_endline "~a~";
     print_endline (string_of_lambda "%" tm2);
     loop()
     | One tm -> print_endline (string_of_lambda "%" tm);
     loop()
     )

let _ = (print_endline "\nWelcome to the Lambda Evaluator"; loop())
