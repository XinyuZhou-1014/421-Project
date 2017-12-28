type lam = 
  | VarLam of string
  | AbsLam of string * lam
  | AppLam of lam * lam

type expr = One of lam | Two of lam * lam

let rec string_of_lambda lambda tm =
    (match tm with VarLam s -> s
     | AbsLam (s,e) -> lambda^" "^s^". "^(string_of_lambda lambda e)
     | AppLam (rator, rand) ->
       (match rator
        with AbsLam (_,_) -> "("^(string_of_lambda lambda rator)^")"
         | _ -> string_of_lambda lambda rator) ^ " " ^
       (match rand with VarLam s -> s
         | _ ->  "("^(string_of_lambda lambda rand)^")"))

type binding_relation = 
  | Bind of int * int list 
  | Free of int * int list
  (* for bind, lead int is binder; 
   * for free, lead int is marker for easy search *)


(* 
env = (int: counter,
 *     (str * int ) list : map, 
 *     binding_relation list: binding) 
 *)

let rec find_in_map map var = 
  match map with
  | [] -> None
  | (str, n)::xs -> if str = var then Some n 
                    else find_in_map xs var

let update_map map var counter = (var, counter) :: map

let add_binding_in_binding_rel bind_list counter = 
  bind_list @ [Bind(counter, [])]
(* only binding var call this *)

let add_newfree_in_binding_rel bind_list counter = 
  bind_list @ [Free(counter, [])]

let update_binding_rel bind_list counter int_of_var = 
  let rec helper left right counter int_of_var =  
    match right with 
    | [] -> raise (Failure "This should not happen")
    | (Bind(n, lst)) :: xs -> 
        if n = int_of_var then left @ [Bind(n, lst @ [counter])] @ xs
        else helper (left @ [Bind(n, lst)]) xs counter int_of_var 
    | (Free(n, lst)) :: xs -> 
        if n = int_of_var then left @ [Free(n, lst @ [counter])] @ xs
        else helper (left @ [Free(n, lst)]) xs counter int_of_var 
  in helper [] bind_list counter int_of_var

let add_new_lambda_var env var =
  match env with (counter, bind_list, map) ->
    let new_counter = counter + 1 in
    let new_map = update_map map var new_counter in
    let new_bind_list = add_binding_in_binding_rel bind_list new_counter in
    (new_counter, new_bind_list, new_map)

let add_new_var env var = 
  match env with (counter, bind_list, map) ->
    let new_counter = counter + 1 in
    let int_of_var_op = find_in_map map var in
    (match int_of_var_op with 
    | None -> (* new free *)
        let new_bind_list = add_newfree_in_binding_rel bind_list new_counter in
        let new_map = update_map map var new_counter in
        (new_counter, new_bind_list, new_map) 
    | Some int_of_var -> (* bounded *)
        let new_bind_list = update_binding_rel bind_list new_counter int_of_var in
        let new_map = map in
        (new_counter, new_bind_list, new_map)
    )


let rec get_binding_relation_helper tm env =
  match env with (_, _, old_map) ->
    (match tm with 
     | VarLam s -> 
         (match add_new_var env s with
         | (new_counter, new_bind_list, new_map) ->
             (new_counter, new_bind_list, old_map)
         )

     | AbsLam (s,e) -> 
         let new_env = add_new_lambda_var env s in
         (match get_binding_relation_helper e new_env with
         | (new_counter, new_bind_list, new_map) ->
             (new_counter, new_bind_list, old_map)
         )

     | AppLam (rator, rand) ->
         let new_env = get_binding_relation_helper rator env in
         let new_new_env = get_binding_relation_helper rand new_env in
         (match new_new_env with
         | (new_counter, new_bind_list, new_map) ->
             (new_counter, new_bind_list, old_map)
         )
    )

let get_binding_relation tm = 
  let env = (0, [], []) in
  match get_binding_relation_helper tm env with
  | (_, bind_list, _) -> bind_list

