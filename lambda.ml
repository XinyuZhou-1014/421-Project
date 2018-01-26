(* Use for getting binding relationship from typed lambda term. *)
(* E.G: Given (%x. (%y.y) x) y, 
 * first convert into typed lambda (by parser lambda_parser):
 * AppLam(
 *   AbsLam(x, (
 *     AppLam(
 *       AbsLam(y, VarLam(y)),
 *       x
 *     )
 *   ), y
 * )
 * Then convert the lambda type into binding relationship list:
 * [Bind(1, [4], x), Bind(2, [3], y), Free(5, y)] 
 *)
(* Main function: get_binding_relation lambda_ty   *)

open Printf;;

type lam = 
  | VarLam of string        (* x *)
  | AbsLam of string * lam  (* %x.x *)
  | AppLam of lam * lam     (* x y *)

type expr = One of lam | Two of lam * lam

(* type map: a list of (string * int) 
 * where string stands for name of a variable, 
 * int stands for indentifier of the variable (id_of_var)
 * the identifier are the index of the var when it first appears *) 

type binding_relation = 
  | Bind of int * int list * string  
  | Free of int * string
  (* string is the name of var;
   * for bind, lead int is identifier of the binder var, list is the index of bounded vars; 
   * for free, simply record position and string *)

(* env = (int: counter, 
 *        (str * int) list : map, 
 *        binding_relation list: binding relationship) 
 *)
(* counter: count the number of var in the env, and become the index of next var; 
 * also be the identifier of a var if it first appears in the map *)

(* find the variable in map *)
let rec find_in_map map var = 
  match map with
  | [] -> None
  | (str, n)::xs -> if str = var then Some n 
                    else find_in_map xs var

(* update the map to let a name of var maps to a new variable identifier *)
(* identifier is simply the counter (i.e. index) of the newly appeared var *)
let update_map map var counter = (var, counter) :: map
(* simply push to the beginning of list, 
 * when searching, the most front one will be picked, 
 * so if a name is reused as a different variable, the identifier of the new one appears
 * before the old one, and will be selected when the map is scanned *)

let add_binding_in_bind_rel bind_list counter str = 
  bind_list @ [Bind(counter, [], str)]
(* call only when adding a new binding var *)

let add_newfree_in_bind_rel bind_list counter str = 
  bind_list @ [Free(counter, str)]
(* call only when the var cannot be found in map *)

(* put a bounded var in the binding list *)
(* call only when the var is assured in the binding list, 
 * and want the find the binding variable for it *)
let add_bounded_in_bind_rel bind_list counter id_of_var = 
  (* split the list into two part, in order to replace one term of the list, 
   * i.e. the head of the list_to_scan *)
  let rec helper list_scanned list_to_scan counter id_of_var =  
    match list_to_scan with 
    | [] -> raise (Failure "This should not happen") (* should be one binding var for the var *)
    (* if next is Free, simply skip *)
    | (Free(id, s)) :: xs -> 
        helper (list_scanned @ [Free(id, s)]) xs counter id_of_var 
    (* if next is Bind and the id matches, append the counter of the var to the bounded list
     * else, skip it *)
    | (Bind(id, bounded, str)) :: xs -> 
        if id = id_of_var then list_scanned @ [Bind(id, bounded @ [counter], str)] @ xs
        else helper (list_scanned @ [Bind(id, bounded, str)]) xs counter id_of_var 
  in helper [] bind_list counter id_of_var

(* if the var is a binding var, update the map and add binding in bind_list*)
let add_new_binding_var env var =
  match env with (counter, bind_list, map) ->
    let new_counter = counter + 1 in  (* count num of vars, also as the id of the var *)
    let new_map = update_map map var new_counter in  (* update map *)
    let new_bind_list = add_binding_in_bind_rel bind_list new_counter var in  (* add binding *)
    (new_counter, new_bind_list, new_map)  (* return the new environment *)

(* add bounded or free var *)
let add_new_var env var = 
  match env with (counter, bind_list, map) ->
    let new_counter = counter + 1 in
    let id_of_var_option = find_in_map map var in
    (match id_of_var_option with 
    | None -> (* if not in map, new free *)
        let new_bind_list = add_newfree_in_bind_rel bind_list new_counter var in
        let new_map = update_map map var new_counter in
        (new_counter, new_bind_list, new_map)
    | Some id_of_var -> (* if in map, bounded *)
        let new_bind_list = add_bounded_in_bind_rel bind_list new_counter id_of_var in
        let new_map = map in  (* map not change *)
        (new_counter, new_bind_list, new_map)
    )

(* main logic of getting binding relation *)
let rec get_binding_relation_helper lambda_ty env =
  (* save old map, because the map change in sub envs should not influence the map in root *)
  match env with (_, _, old_map) ->  
    (match lambda_ty with 
     | VarLam s ->  (* simply add new var, recover map, return env *)
         (match add_new_var env s with
         | (new_counter, new_bind_list, new_map) ->
             (new_counter, new_bind_list, old_map)
         )
     | AbsLam (s,e) ->  (* add binding, recursive, recover map, return env *)
         let new_env = add_new_binding_var env s in
         (match get_binding_relation_helper e new_env with
         | (new_counter, new_bind_list, new_map) ->
             (new_counter, new_bind_list, old_map)
         )
     | AppLam (rator, rand) ->  (* TODO: why correct? *)
         let new_env = get_binding_relation_helper rator env in
         let new_new_env = get_binding_relation_helper rand new_env in
         (match new_new_env with
         | (new_counter, new_bind_list, new_map) ->
             (new_counter, new_bind_list, old_map)
         )
    )

(* main function*)
(* init env, get full env after scan the lambda, and return the binding relationship *)
let get_binding_relation lambda_ty = 
  let env = (0, [], []) in
  match get_binding_relation_helper lambda_ty env with
  | (_, bind_list, map) -> bind_list


(* utils*)
(* return the string of lambda term given lambda type.
 * str_of_lam: use what string to denote "lambda" *)
let rec string_of_lambda str_of_lam lambda_ty =
    (match lambda_ty with VarLam s -> s
     | AbsLam (s,e) -> str_of_lam^" "^s^". "^(string_of_lambda str_of_lam e)
     | AppLam (rator, rand) ->
       (match rator
        with AbsLam (_,_) -> "("^(string_of_lambda str_of_lam rator)^")"
         | _ -> string_of_lambda str_of_lam rator) ^ " " ^
       (match rand with VarLam s -> s
         | _ ->  "("^(string_of_lambda str_of_lam rand)^")"))

(* print the binding relationship *)
let rec print_bind_rel bind_list = 
  match bind_list with 
  | [] -> print_endline ""
  | (Bind(id, bounded, str))::xs -> print_string ("Bind(" ^ (string_of_int id) ^ ", ["); 
                    List.iter (printf "%d ") bounded;
                    print_string ("], " ^ str ^ "), ");
                    print_bind_rel xs
  | (Free(id, s))::xs -> print_string ("Free(" ^ (string_of_int id) ^ ", "); 
                    print_string s;
                    print_string "), ";
                    print_bind_rel xs

(* print the typed lambda given lambda string *)
let rec string_of_lambda_by_type lambda_ty = 
    (match lambda_ty with VarLam s -> s
     | AbsLam (s,e) -> "Abs(" ^ s ^ ", " ^ (string_of_lambda_by_type e) ^ ")"
     | AppLam (rator, rand) -> "App" ^
       (match rator
        with AbsLam (_,_) -> "("^(string_of_lambda_by_type rator)^")"
         | _ -> string_of_lambda_by_type rator) ^ " " ^
       (match rand with VarLam s -> s
         | _ ->  "("^(string_of_lambda_by_type rand)^")"))