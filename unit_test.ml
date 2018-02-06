(* Unit Test for one-step alpha equivalance transformation *)
(* read from file, each case has four or five lines, and one empty line netween cases *)
(* first line: conclusion; second line: rule; third [and forth] line: hypothesis(es) *)

open Lambda
open Lambda_parse
open Match_rule


let file = "example.txt"
let is_silent = false


(* put input into OneStepInput, run judge function and print result *)
let test_onestep conclusion_str rule_str hypothesis_list is_silent = 
  let onestep_input = Match_rule.OneStepInput(conclusion_str, rule_str, hypothesis_list) in
  let res = Match_rule.legal_onestep onestep_input
  in if not is_silent then 
  (
    print_endline conclusion_str;
    print_endline rule_str;
    List.map print_endline hypothesis_list;
    print_string "Result: ";
    Match_rule.print_error res;
  )
  else Match_rule.print_error res


  
(* read four or five lines eac time*)
let () =   
  print_endline "";
  let ic = open_in file in
    let rec loop () = 
      try 
        let conclusion_str = input_line ic in  
        let rule_str = input_line ic in
        let line_3 = input_line ic in
        let line_4 = input_line ic in
        let line_5 = input_line ic in 
        let (hypothesis_list, expect_res, skiped_line) = 
            (if (String.length line_5 = 0) (* empty line or not *)
             then ([line_3], line_4, "") 
             else ([line_3; line_4], line_5, input_line ic)) (* one more line (empty line) *)
        in
        test_onestep conclusion_str rule_str hypothesis_list is_silent; 
        print_endline ("Expected: " ^ expect_res);
        print_endline "";
        loop ()
      
      with e -> 
        match e with
        | End_of_file -> 
            close_in_noerr ic; 
            print_endline "Finish." 
        | _ -> 
          close_in_noerr ic;           
          raise e                      
  in loop ()

