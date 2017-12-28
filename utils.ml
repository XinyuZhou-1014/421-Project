(* File: utils.ml *)
(* Author: Elsa L Gunter *)
(* Copyright 2016 *)
(* Share and Enjoy *)

(* This file attempts to develop an equivalence test on right regular
   expressions. *)
let compare = ();;

(* Generic list as as set funcitons *)
let bool_compare b1 b2 =
  match (b1,b2)
  with (false, false) -> 0
    | (false, true) -> -1
    | (true, false) -> 1
    | (true, true) -> 0

let rec mem compare x l =
  match l with [] -> false | (y::ys) -> (compare x y = 0) || mem compare x ys

let pair_compare fst_compare snd_compare (a,b) (x,y) =
  match fst_compare a x
  with 0 -> snd_compare b y
    | c -> c

let rec list_compare compare l1 l2 =
  match (l1,l2)
  with ([],[]) -> 0
    | ([], _) -> -1
    | (_, []) -> 1
    | ((x::xs), (y::ys)) ->
      pair_compare compare (list_compare compare) (x,xs) (y,ys)

let rec insert_uniq compare x lst =
  match lst with [] -> [x]
    | y::ys ->
      let n = compare x y 
      in
      if n < 0 then x::lst
      else if n = 0 then x::ys (*prefer the inserted one*)
      else y::(insert_uniq compare x ys)

let partial_insert_uniq compare xopt lst =
  match xopt with Some x -> insert_uniq compare x lst
    | None -> lst

let sort_uniq compare lst = List.fold_right (insert_uniq compare) lst []

let set_minus compare lst1 lst2 =
  List.filter (fun x -> not(mem compare x lst2)) lst1

let set_diff compare lst1 lst2 =
  (set_minus compare lst1 lst2, set_minus compare lst2 lst1)

let set_union compare lst1 lst2 =
  List.fold_right (insert_uniq compare) lst1 lst2

let string_of_list lb sep rb string_of_elt lst =
  let rec mid_str_of_list l =
      match l with [] -> ""
        | [x] -> string_of_elt x
        | x :: xs -> string_of_elt x ^ sep ^ mid_str_of_list xs
  in lb^(mid_str_of_list lst)^rb

let assoc_compare compare (key1,val1) (key2,val2) = compare key1 key2

let update_assoc compare = insert_uniq (assoc_compare compare)

let rec assoc_lookup compare key1 dictionary =
  match dictionary
  with [] -> None
    | (key2, value)::rem_entries -> 
      if compare key1 key2 = 0 then Some value
      else assoc_lookup compare key1 rem_entries

let string_of_assoc_list string_of_key string_of_value =
  string_of_list "{" "," "}"
    (fun (key,value) -> ((string_of_key key)^" |-> "^(string_of_value value)))
