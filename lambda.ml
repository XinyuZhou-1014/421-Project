(* File: lambda.ml *)
(* Author: Elsa L. Gunter *)
(* Copyright: 2016 *)
(* Share and Enjoy *)

open Utils

type lam = Var of string | Abs of (string * lam) | App of (lam * lam)

let rec string_of_lambda lambda tm =
    (match tm with Var s -> s
     | Abs (s,e) -> lambda^" "^s^". "^(string_of_lambda lambda e)
     | App (rator, rand) ->
       (match rator
        with Abs (_,_) -> "("^(string_of_lambda lambda rator)^")"
         | _ -> string_of_lambda lambda rator) ^ " " ^
       (match rand with Var s -> s
         | _ ->  "("^(string_of_lambda lambda rand)^")"))


let int_to_string n =
    let int_to_int_26_list n =
        let rec aux n l =
            if n <= 0 then l else let c = ((n-1) mod 26) in aux ((n -(c+1))/26) (c::l)
        in aux n []
    in
        let rec aux l = match l with [] -> ""
                            | n::ns -> (String.make 1 (Char.chr (n + 97))) ^ aux ns
        in aux (int_to_int_26_list n)

let freshFor lst = 
    let rec fresh_ n = 
        if mem String.compare (int_to_string n) lst
           then fresh_ (n+1)
        else int_to_string n
    in fresh_ 1

let rec lambda_compare lam1 lam2 =
  match (lam1, lam2)
  with (Var x, Var y) -> String.compare x y
    | (Var _ , _ ) -> -1
    | (_, Var _) -> 1
    | (Abs (x,e1), Abs (y, e2)) ->
      pair_compare String.compare lambda_compare (x,e1) (y,e2)
    | (Abs (_,_), _ ) -> -1
    | (_, Abs (_,_)) -> 1
    | (App(e1,e2),App(e3,e4)) ->
      pair_compare lambda_compare lambda_compare (e1,e2) (e3,e4)

let rec vars_in_lambda lambda =
  match lambda
  with Var x -> [x]
    | Abs (x,e) -> insert_uniq String.compare x (vars_in_lambda e)
    | App (e1,e2) -> set_union String.compare (vars_in_lambda e1) (vars_in_lambda e2)

let rec free_vars lambda =
  match lambda
  with Var x -> [x]
    | Abs (x,e) -> set_minus String.compare (free_vars e) [x]
    | App (e1,e2) -> set_union String.compare (free_vars e1) (free_vars e2)

let string_swap x y u = if u = y then x else if u = x then y else u

let rec swap x y e =
  match e
  with Var u -> Var (string_swap x y u)
    | App (e1, e2) -> App (swap x y e1, swap x y e2)
    | Abs (z,body) -> Abs (string_swap x y z, swap x y body)

(* Is there any point to var subst given swap and sub? *)
let rec var_subst x y e = (* x for free y in e *)
  match e
  with Var u -> Var (if u = y then x else u)
    | App (e1, e2) -> App(var_subst x y e1, var_subst x y e2)
    | Abs (z,body) ->
      if z = y then e (* Abs(y,body) *)
      else if z = x then Abs (y, swap x y body)
      else Abs(z, var_subst x y body)

let rec alpha_equiv_error (lam1,lrenaming) (lam2,rrenaming) =
  let default = Some((lam1,lrenaming),(lam2,rrenaming))
  in
  match (lam1, lam2)
  with (Var x, Var y) ->
    if (x = y) then None else default
    | (Var _, _) -> default
    | (_ , Var _) -> default
    | (App(e1,e2), App(e3,e4)) ->
      (match alpha_equiv_error (e1, lrenaming) (e3, rrenaming)
       with None -> alpha_equiv_error (e2, lrenaming) (e4, lrenaming)
         | r -> r)
    | (App(_,_), _) -> default
    | (_, App(_,_)) -> default
    | (Abs(x,e1), Abs(y,e2)) ->
      if x = y then alpha_equiv_error  (e1, lrenaming) (e2, rrenaming)
(* *)
       else
          let z =
            freshFor
              (set_union String.compare
                 (vars_in_lambda lam1) (vars_in_lambda lam2))
          in
          alpha_equiv_error
            (swap x z e1, update_assoc String.compare (z,x) lrenaming)
            (swap y z e2, update_assoc String.compare (z,y) rrenaming) 
(*
      else
        alpha_equiv_error (update_assoc String.compare (y,x) renaming) e1 e2
*)


let rec rename renaming tm =
  let rn v =
    match assoc_lookup String.compare v renaming
    with Some x -> x | None -> v
  in
  match tm
  with Var x -> Var (rn x)
    | App(e1,e2) -> App(rename renaming e1, rename renaming e2)
    | Abs(x,e) -> Abs(rn x, rename renaming e)

      
exception FreeVariableCapture of (lam * string * lam) 
(* "substituting "^residue^" for "^redex^" in "^lambda^
   "will cause free variable capture.\n  You need to use $\alpha$-conversion first.\n"*)

let rec sub r term =
    (match r with (residue, redex) ->
      (match term
       with Var s -> if s = redex then residue else term
         | App (e1, e2) -> App ((sub r e1), (sub r e2))
         | Abs (s, e) ->
           if s = redex then term
           else if mem String.compare s (free_vars residue) &&
               mem String.compare redex (free_vars e)
                then raise (FreeVariableCapture (residue, redex, term))
                else  (Abs (s, (sub r e)))))

type step_result =
    Out of (lam * lam list)
  | NeedsAlpha of (lam * string * lam)
  | InputDone

let rec eager_one_step tm = 
  match tm with
 (Var x) -> InputDone (* shouldn't happen *)
| (Abs (s, e)) -> InputDone
| (App (rator, rand)) ->
  (match eager_one_step rator
   with InputDone ->
     (match rator
      with Abs (x, f) -> 
        (match eager_one_step rand
         with InputDone ->
           (try Out (sub (rand,x) f,[])
            with FreeVariableCapture (residue, redex, term) ->
              NeedsAlpha (residue, redex, term))
           | Out (newrand,lst) ->
             Out (App (rator, newrand),
                  List.map (fun rnd -> App(rator, rnd)) lst)
           | NeedsAlpha (residue, redex, term) as r -> r)
        | _ -> InputDone)
     | Out (newrator,lst) ->
       Out (App (newrator, rand), List.map (fun rtr -> App(rtr,rand)) lst)
     | NeedsAlpha (residue, redex, term) as r -> r)
(*
let rec eager_one_step tm = 
  match tm with
 (Var x) -> None (* shouldn't happen *)
| (Abs (s, e)) -> None
| (App (rator, rand)) ->
  (match rator with Var x -> None (* shouldn't happen *)
   | Abs (x, f) -> 
     (match eager_one_step rand with None ->
       (match sub (rand,x) with Non Some ( f))
       | Some newrand -> Some (App (rator, newrand)))
   | App (_,_) ->
     (match eager_one_step rator with None -> None
       | Some newrator -> Some (App (newrator, rand))))
*)

let rec lazy_one_step tm = 
  match tm with
 (Var x) -> InputDone 
| (Abs (s, e)) -> InputDone
| (App (rator, rand)) ->
  (match lazy_one_step rator
   with InputDone ->
     (match rator
      with Abs (x, f) ->
        (try (Out ((sub (rand,x) f),[]))
         with FreeVariableCapture (residue, redex, term) ->
           NeedsAlpha (residue, redex, term))
        | _ -> InputDone)
     | Out (newrator,lst) ->
       Out (App (newrator, rand), List.map (fun rtr -> App(rtr,rand)) lst)
     | NeedsAlpha (residue, redex, term) as r -> r)

(*
let rec lazy_one_step tm = 
  match tm with
 (Var x) -> None (* shouldn't happen *)
| (Abs (s, e)) -> None
| (App (rator, rand)) ->
  (match rator with Var x -> None (* shouldn't happen *)
   | Abs (x, f) -> (Some (sub (rand,x) f))
   | App (_,_) ->
     (match lazy_one_step rator with None -> None
       | Some newrator -> Some (App (newrator, rand))))
*)


let rec mk_clean_alpha avoid tm =
  match tm
  with Var a -> Var a
    | App(e1, e2) -> App(mk_clean_alpha avoid e1, mk_clean_alpha avoid e2)
    | Abs(x,e) ->
      if mem String.compare x avoid
      then
        let y = freshFor (set_union String.compare (vars_in_lambda e) avoid)
        in
        Abs(y,mk_clean_alpha (insert_uniq String.compare y avoid) (swap x y e))
      else Abs(x,mk_clean_alpha (insert_uniq String.compare x avoid) e)

let rec output_eval lambda evalf tm =
  (print_string (string_of_lambda lambda tm);
   match evalf tm with InputDone -> print_newline();
     | Out (newtm, others) -> (print_string "\n\\\\ \\betaarrow\n";
                     output_eval lambda evalf newtm)
     | NeedsAlpha (residue, redex, term) ->
       (print_string "\n\\\\ \\alphaequiv\n";
        output_eval lambda evalf
          (mk_clean_alpha (free_vars residue) tm)) );;

let rec diff_one tm1 tm2 =
  match (tm1, tm2)
  with (Var x, Var y) -> if x = y then None else Some (tm1, tm2)
    | (Abs (x,b), Abs (y, c)) ->
      if x = y then diff_one b c else Some (tm1, tm2)
    | (App (a,b), App(c,d)) ->
        (match diff_one a c
         with None -> diff_one b d
           | r -> r)
    | _ -> Some (tm1, tm2)

type diff_result = NoDiff | OneDiff of (lam * lam) | MultiChoiceDiff

let rec difference tm tms =
  let diffs = List.map (diff_one tm) tms
  in
  if List.exists (fun x -> match x with None -> true | _ -> false) diffs
  then NoDiff
  else 
    match diffs
    with [Some(tm1,tm2)] -> OneDiff(tm1,tm2)
    | _ -> MultiChoiceDiff

let ne_map f (first,rest) = (f first, List.map f rest)
let ne_append (f1,r1) (f2,r2) = (f1, (r1 @ (f2::r2)))
  
let rec beta_one_step tm=
  match tm
  with Var x -> InputDone
    | App (e1, e2) ->
      let e1_inner_betas =
        (match beta_one_step e1
         with InputDone -> InputDone
           | Out res1 -> Out (ne_map  (fun e1' -> App(e1',e2)) res1)
           | NeedsAlpha(s,x,t) as r -> r )
      in
      let e2_inner_betas =
        (match beta_one_step e2
         with InputDone -> InputDone
           | Out res2 -> Out (ne_map  (fun e2' -> App(e1,e2')) res2)
           | NeedsAlpha(s,x,t) as r -> r )
      in
      let outer_beta =
        (match e1
         with Abs(x, f) ->
           (try Out ((sub (e2,x) f), [])
            with FreeVariableCapture (residue, redex, term) ->
              NeedsAlpha (residue, redex, term))
           | _ -> InputDone)
      in
        (match (outer_beta, e1_inner_betas, e2_inner_betas) 
         with (Out top, Out res1, Out res2) ->  Out (ne_append top (ne_append res1 res2))
           | (Out top, Out res1, _) -> Out (ne_append top res1)
           | (Out top, _, Out res2) -> Out (ne_append top res2)
           | (_, Out res1, Out res2) -> Out (ne_append res1 res2)
           | (Out top as t, _, _) -> t
           | (_, (Out res1 as r1), _) -> r1
           | (_, _, (Out res2 as r2)) -> r2
           | ((NeedsAlpha(s,x,t) as r), _, _) -> r
           | (_, (NeedsAlpha(s,x,t) as r), _) -> r
           | (_, _, (NeedsAlpha(s,x,t) as r)) -> r
           | _ -> InputDone)
    | Abs (x, e) ->
      (match beta_one_step e
       with InputDone -> InputDone
         | NeedsAlpha(s,x,t) as r -> r
         | Out res -> Out(ne_map (fun e' -> Abs(x,e')) res))


type result = Success of bool * int | Failed of string | ParseError of string

type tag = Alpha | Beta

type evalf = Eager | Lazy | AlphaBeta

let mk_evalf evalf =
  match evalf
  with Eager -> eager_one_step
    | Lazy -> lazy_one_step
    | AlphaBeta -> beta_one_step
    
let checkOneStepEval lambda beta_value lhs evalf rhs =
  match mk_evalf evalf lhs
  with InputDone ->
    Failed "This should not happen as evaluation is already complete.\n"
    | NeedsAlpha (residue, redex, term) ->
      Failed ("Your attempt at beta reduction requires a use of alpha\n"^
                 "conversion first to avoid free variable capture.\n"
(*        "Substituting "^(string_of_lambda lambda residue)^" for "^
                 redex^" in \n"^(string_of_lambda lambda term)^
                 "\n will cause free variable capture.\n"^
                 "  You need to use $\\alpha$-conversion first.\n"*))
    | Out (first,rest) ->
      (match difference rhs (first::rest)
       with NoDiff ->
         (match mk_evalf evalf rhs
          with InputDone -> Success (true,beta_value)
            | _ -> Success (false,beta_value))
         | OneDiff(tm1,tm2) ->
           Failed ("Incorrect attempt and a beta reduction")
                      (*"You have an instance of \n"^
                      (string_of_lambda lambda tm1)^
                      "\n where an instance of \n"^
                      (string_of_lambda lambda tm2)^
                      "\n was needed instead.\n"*)
         | MultiChoiceDiff ->
           Failed ("There were multiple possibilities for this step, but\n"^
                      "your answer differs from all of them.\n"))

let checkInput lambda evalf alpha_value beta_value (lhs, tag, rhs) =
  match tag
  with Alpha ->
    (match alpha_equiv_error (lhs,[]) (rhs,[])
     with None -> 
       (match mk_evalf evalf rhs
        with InputDone -> Success (true,alpha_value)
          | _ -> Success (false,alpha_value))
       | Some ((tm1,lrenaming), (tm2, rrenaming)) ->
         Failed ("Incorrect attempt at alpha equivalence.\n")
(*((string_of_lambda lambda lhs)^" is not alpha equivalent to \n"^
                    (string_of_lambda lambda rhs)^
                    (*(string_of_lambda lambda (rename lrenaming tm1))^" is not alpha equivalent to "^
                    (string_of_lambda lambda (rename rrenaming tm2))^"\n under the renaming\n "^
                    (string_of_assoc_list (fun s -> s) (fun s -> s) renaming)^*)
  "\n")*)
    )
    | Beta -> checkOneStepEval lambda beta_value lhs evalf rhs


(*
let tm = App
 (Abs ("x", App (Var "x", Abs ("y", App (Var "x", Var "y")))),
  App (Abs ("u", Var "u"), Abs ("w", Var "w")))
*)
