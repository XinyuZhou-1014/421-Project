(* File: lambdaTest.ml *)
(* Author: Elsa L. Gunter *)
(* Copyright: 2016 *)
(* Share and Enjoy *)

open Lambda
open Lambda_parse
open Match_operation

(*
let str = "(% x. y % y. y (% x. x y) x) x;;";;
print_endline str;;

let _ = 
  let test = 
    Lambda_parse.input Lambda_lex.token 
    (Lexing.from_string str)
  in 
  match test with
  | One lam -> 
      Lambda.print_binding_rel (Lambda.get_binding_relation lam)
  | Two (lam1, lam2) -> 
      Lambda.print_binding_rel (Lambda.get_binding_relation lam1);
      print_endline "~a~";
      Lambda.print_binding_rel (Lambda.get_binding_relation lam2)
*)

let _ = 
  let op = LeftConvOp in
  let str1 = "(%x.x (%y.y x z) w x z) x ~a~ (%y.y(%w.w y z) w y z) x;;" in
  let str2 = "(%x.x (%w.w x z) w x z) x ~a~ (%y.y(%w.w y z) w y z) x;;" in
  let res = legal_onestep op [str1] [str2] in
  Match_operation.print_error res



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

let _ = (print_endline "\nWelcome to the Lambda Evaluator"; loop())
