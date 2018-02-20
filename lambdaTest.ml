(* Interactive Test Environment *)
(* Input one line as conclusion, one line as rule, and few line(s) as hypothesis *)

open Lambda
open Lambda_parse
open Match_rule

(* get result *)
let test_onestep conclusion_str rule_str hypothesis_list = 
  print_endline conclusion_str;
  print_endline rule_str;
  List.map print_endline hypothesis_list;
  let onestep_input = Match_rule.OneStepInput(conclusion_str, rule_str, hypothesis_list) in
  let res = Match_rule.legal_onestep onestep_input in
  print_string "Result: ";
  Match_rule.print_error res

(* Loop for hypothesis *)
let rec loop_helper hypothesis_list = 
  print_endline "Input Hypothesis ('Enter' if finished)> ";
  let hypo_or_end = input_line stdin in
  if hypo_or_end = "" then hypothesis_list else
  let hypothesis_list = hypothesis_list @ [hypo_or_end] in
  loop_helper hypothesis_list

(* main loop *)
let rec loop () = 
  print_endline "\nInput Conclusion> ";
  let conclusion_str = input_line stdin in
  print_endline "Input Rule>";
  let rule_str  = input_line stdin in 
  let hypothesis_list = loop_helper [] in
  test_onestep conclusion_str rule_str hypothesis_list


let _ = 
  (
  print_endline "\nWelcome to the Alpha Equivalence One-Step Evaluator"; 
  print_endline "Example: ";
  
  let rule_str = "LeftConvRule" in
  let conclusion_str = "(%x.x (%y.y x z) w x z) x ~a~ (%y.y(%w.w y z) w y z) x" in
  let hypothesis = "(%x.x (%w.w x z) w x z) x ~a~ (%y.y(%w.w y z) w y z) x" in
  test_onestep conclusion_str rule_str [hypothesis];
  
  loop()
  )
