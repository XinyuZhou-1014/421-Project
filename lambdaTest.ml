(* File: lambdaTest.ml *)
(* Author: Elsa L. Gunter *)
(* Copyright: 2016 *)
(* Share and Enjoy *)

open Lambda
open Lambda_parse
open Match_rule


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
                 print_endline (string_of_lambda_by_type tm);
                 Lambda.print_binding_rel (Lambda.get_binding_relation tm);
     loop()
     )

let _ = 
  (
  print_endline "\nWelcome to the Alpha Equivalence One-Step Evaluator"; 
  print_endline "Example: ";
  
  let rule = LeftConvRule in
  let str1 = "(%x.x (%y.y x z) w x z) x ~a~ (%y.y(%w.w y z) w y z) x" in
  let str2 = "(%x.x (%w.w x z) w x z) x ~a~ (%y.y(%w.w y z) w y z) x" in
  print_endline str1; 
  Match_rule.print_rule rule;
  print_endline str2;
  let res = legal_onestep rule [str1] [str2] in
  Match_rule.print_error res;
  
  loop()
  )
