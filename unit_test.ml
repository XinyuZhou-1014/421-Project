open Lambda
open Lambda_parse
open Match_operation


let test_onestep str1 op str2 is_silent =
  let res = 
    Match_operation.legal_onestep op [str1] [str2] 
  in if not is_silent then 
  (
    print_endline str1;
    Match_operation.print_op op;
    print_endline str2;
    Match_operation.print_error res;
  )
  else Match_operation.print_error res


let test_onestep_full_app str1 op str2 str3 is_silent =
  let res = 
    Match_operation.legal_onestep op [str1] [str2; str3] 
  in if not is_silent then 
  (
    print_endline str1;
    Match_operation.print_op op;
    print_endline str2;
    print_endline str3;
    Match_operation.print_error res;
  )
  else Match_operation.print_error res

  
let file = "example.txt"
  
let () =   
  print_endline "";
  let is_silent = true in
  let ic = open_in file in
    let rec loop () = 
      try 
        let str1 = input_line ic in  
        let op_str = input_line ic in
        let str2 = input_line ic in
        let expect_res = input_line ic in
        let skiped_line = input_line ic in
        let op = Match_operation.str_2_op op_str in
        test_onestep str1 op str2 is_silent; 
        print_endline expect_res;
        print_endline "";
        loop ()
      
      with e ->                      
        close_in_noerr ic;           
        raise e                      
  in loop ()

