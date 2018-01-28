open Lambda
open Lambda_parse
open Match_rule

(* OneStepInput: str * rule * str list *)

let test_onestep conclusion_str rule_str hypothesis_list is_silent =
  let rule = Match_rule.str_2_rule rule_str in
  let onestep_input = Match_rule.OneStepInput(conclusion_str, rule, hypothesis_list) in
  let res = Match_rule.legal_onestep onestep_input
  in if not is_silent then 
  (
    print_endline conclusion_str;
    Match_rule.print_rule rule;
    List.map print_endline hypothesis_list;
    print_string "Result: ";
    Match_rule.print_error res;
  )
  else Match_rule.print_error res

  
let file = "example.txt"
  
let () =   
  print_endline "";
  let is_silent = false in
  let ic = open_in file in
    let rec loop () = 
      try 
        let conclusion_str = input_line ic in  
        let rule_str = input_line ic in
        let line_3 = input_line ic in
        let line_4 = input_line ic in
        let line_5 = input_line ic in 
        let (hypothesis_list, expect_res, skiped_line) = 
            (if (String.length line_5 < 3) 
             then ([line_3], line_4, "") 
             else ([line_3; line_4], line_5, input_line ic))
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

