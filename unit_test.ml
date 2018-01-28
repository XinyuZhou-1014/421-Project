open Lambda
open Lambda_parse
open Match_rule


let test_onestep str1 rule str2 is_silent =
  let onestep_input = Match_rule.OneStepInput(str1, rule, [str2]) in
  let res = Match_rule.legal_onestep onestep_input
  in if not is_silent then 
  (
    print_endline str1;
    Match_rule.print_rule rule;
    print_endline str2;
    Match_rule.print_error res;
  )
  else Match_rule.print_error res


let test_onestep_full_app str1 rule str2 str3 is_silent =
  let onestep_input = Match_rule.OneStepInput(str1, rule, [str2; str3]) in
  let res = Match_rule.legal_onestep onestep_input
  in if not is_silent then 
  (
    print_endline str1;
    Match_rule.print_rule rule;
    print_endline str2;
    print_endline str3;
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
        let str1 = input_line ic in  
        let rule_str = input_line ic in
        let str2 = input_line ic in
        let expect_res = input_line ic in
        let skiped_line = input_line ic in
        let rule = Match_rule.str_2_rule rule_str in
        print_string "Result: "; 
        test_onestep str1 rule str2 is_silent; 
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

