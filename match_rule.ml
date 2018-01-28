(* Rules for judge one-step alpha equivalence transformation *)
(* Including: Transition, 
              Alpha-conversion (left, right), 
              Abstraction, 
              Application (left, right, both) 
*)
(* Main function: legal_onestep onestep_input *)

open Lambda;;
open Lambda_parse;;


type rule =
  | LeftConvRule
  | RightConvRule
  | AbsRule
  | AppRule
  | LeftAppRule
  | RightAppRule
  | TransitionRule
  | UnknownRule
(* 
 * rule: type of Rule
 * conclusion: have only one element, i.e. "left ~a~ right" string
 * hypothesis_list: have one or two elements, each element is a "left ~a~ right" string
 * rules:
   * a_conv: 1 -> 1
   * abs: 1 -> 1 (lambda term should be equal)
   * app: 1 -> 2
   * l/r app: 1 -> 1
   * trans: 1 -> 2
 *)

type onestep_input = 
  | OneStepInput of string * rule * string list (* conclusion, rule, hypothesis list *)

type error = 
  | NoError
  | NotImplemented
  | NotParsedError (* how to raise? *)
  | NumOfHypoError
  | UndefinedError
  | WrongRule
  | UnNamedError of string
  (* alpha conversion *)
  | LeftConvRightNotSame
  | RightConvLeftNotSame
  | AlphaConvWrong
  | AlphaConvNotChange
  | AlphaConvChangeTooMany
  (* abs error *)
  | LambdaTermNotSame 
  | AbsRewriteWrong
  | LeftAppRightNotSame
  | RightAppLeftNotSame
  (* app error *)
  | AppLeftRewriteWrong
  | AppRightRewriteWrong
  (* transition error *)
  | TransitionRewriteError
  (* conv not good, can't abs, can't app, 
   * abs lambda not equal, abs rewrite wrong, 
   * left app right not same, 
   * right app left not same,
   * app left rewrite wrong, app right rewrite wrong*)


(* parse *)
let parse_exp str = 
  let expr = Lambda_parse.input Lambda_lex.token
             (Lexing.from_string str)
  in match expr with
  | One lam -> raise(Failure "should have two here")
  | Two (left, right) -> (left, right)

 
(* higher logic used in the rule judgment *)

(* judge whether two typed lambda are the same *)
let rec same_lambda e1 e2 = match (e1, e2) with
  | (VarLam s1, VarLam s2) -> s1 = s2
  | (AbsLam (s1, r1), AbsLam (s2, r2)) ->
      s1 = s2 && same_lambda r1 r2
  | (AppLam (l1, r1), AppLam(l2, r2)) ->
      same_lambda l1 l2 && same_lambda r1 r2
  | (_, _) -> false

(* Count the total number of difference in binding relation list *)
(* Count += 1 iff two binds have difference only on the name of var *)
(* Any other difference will lead to None *)
(* If no other difference, return Some n where n is the total number of difference *)
(* Use for judge the legal alpha conversion (has only one difference) *)
let rec count_bind_list_change l1 l2 = match (l1, l2) with
  | ([], []) -> Some 0
  | (Bind(id_1, lst1, str_1)::xs, Bind(id_2, bounded_2, str_2)::ys) -> 
      (match count_bind_list_change xs ys with
      | None -> None
      | Some n -> 
          if not (id_1 = id_2) then None
          else if str_1 = str_2 then Some n 
               else Some (n+1)
      )
  | (Free(id_1, s1)::xs, Free(id_2, s2)::ys) ->
      if (id_1 = id_2 && s1 = s2) then count_bind_list_change xs ys
      else None
  | (_, _) -> None

(* judge the legal alpha conversion (has only one difference) *)
let is_alpha_conversion lam1 lam2 = 
  match count_bind_list_change 
        (Lambda.get_binding_relation lam1) 
        (Lambda.get_binding_relation lam2)
  with 
  | None -> AlphaConvWrong 
  | Some n -> if n = 1 then NoError else
              if n = 0 then AlphaConvNotChange else
              AlphaConvChangeTooMany 


(* rules *)

(* most of the rules go through rule_template, which accept a helper function, 
 * make some match on the inputs and pass them to the helper *)
let rule_template judge_helper conclusion hypothesis_list =   
  match hypothesis_list with
  | hypothesis::[] ->
      (match (conclusion, hypothesis) with
      | ((con_left, con_right), (hypo_left, hypo_right)) -> 
          judge_helper con_left con_right hypo_left hypo_right
      | (_, _) -> NotImplemented (* One *)
      )
  | _ -> NumOfHypoError

(* correct only if right part unchanged and left part a valid alpha conversion *)
let legal_con_leftv_helper con_left con_right hypo_left hypo_right = 
  if not (same_lambda con_right hypo_right) then LeftConvRightNotSame else 
  is_alpha_conversion con_left hypo_left

(* similar as above *)
let legal_con_rightv_helper con_left con_right hypo_left hypo_right = 
  if not (same_lambda con_left hypo_left) then RightConvLeftNotSame else 
  is_alpha_conversion con_right hypo_right

(* correct only if binding term are same and bounded term unchanged *)
let legal_abs_helper con_left con_right hypo_left hypo_right = 
  match (con_left, con_right) with
  | (AbsLam(s_con_left, e_con_left), AbsLam(s_con_right, e_con_right)) -> 
      if not (s_con_left = s_con_right)           then LambdaTermNotSame else 
      if not (same_lambda e_con_left hypo_left)   then AbsRewriteWrong else 
      if not (same_lambda e_con_right hypo_right) then AbsRewriteWrong else 
      NoError
  | _ -> WrongRule 

(* correct if all the parts are rewrite correctly *)
(* do not use rule_template *)
let legal_app conclusion hypothesis_list = 
  match (conclusion, hypothesis_list) with
  | ((con_left, con_right), 
     (fst_hypo_left, fst_hypo_right)::(snd_hypo_left, snd_hypo_right)::[]) -> 
    (match (con_left, con_right) with
    | (AppLam(con_left_rator, con_left_rand), AppLam(con_right_rator, con_right_rand)) ->
      if not (same_lambda con_left_rator fst_hypo_left)   then AppLeftRewriteWrong else 
      if not (same_lambda con_left_rand snd_hypo_left)   then AppLeftRewriteWrong else 
      if not (same_lambda con_right_rator fst_hypo_right) then AppRightRewriteWrong else 
      if not (same_lambda con_right_rand snd_hypo_right) then AppRightRewriteWrong else 
      NoError 
    | _ -> WrongRule 
    )
  | (conlusion, _::_::[]) -> NotImplemented (* One *)
  | _ -> NumOfHypoError

(* check left part rewrite *)
let legal_left_app_helper con_left con_right hypo_left hypo_right = 
  match (con_left, con_right) with
  | (AppLam(con_left_rator, con_left_rand), AppLam(con_right_rator, con_right_rand)) ->
      if not (same_lambda con_left_rator hypo_left)    then AppLeftRewriteWrong else 
      if not (same_lambda con_right_rator hypo_right)  then AppLeftRewriteWrong else 
      if not (same_lambda con_left_rand con_right_rand) then LeftAppRightNotSame else
      NoError 
  | _ -> WrongRule 

(* check right part rewrite *)
let legal_right_app_helper con_left con_right hypo_left hypo_right = 
  match (con_left, con_right) with
  | (AppLam(con_left_rator, con_left_rand), AppLam(con_right_rator, con_right_rand)) ->
      if not (same_lambda con_left_rand hypo_left)    then AppRightRewriteWrong else 
      if not (same_lambda con_right_rand hypo_right)  then AppRightRewriteWrong else 
      if not (same_lambda con_left_rator con_right_rator) then RightAppLeftNotSame else 
      NoError 
  | _ -> WrongRule 

(* exp1 ~a~ exp3 -> [exp1 ~a~ exp2; exp2 ~a~ exp3] *)
let legal_transition conclusion hypothesis_list = 
  match (conclusion, hypothesis_list) with
  | ((con_left, con_right), 
     (fst_hypo_left, fst_hypo_right)::(snd_hypo_left, snd_hypo_right)::[]) -> 
      if not (same_lambda con_left fst_hypo_left)       then TransitionRewriteError else
      if not (same_lambda fst_hypo_right snd_hypo_left) then TransitionRewriteError else
      if not (same_lambda con_right snd_hypo_right)     then TransitionRewriteError else
      NoError 
  | (conlusion, _::_::[]) -> NotImplemented (* One *)
  | _ -> NumOfHypoError

(* main function *)
let legal_onestep onestep_input =
  (match onestep_input with OneStepInput(con_str, rule, hypo_str_list) -> 
    let conclusion = parse_exp con_str in 
    let hypothesis_list = List.map parse_exp hypo_str_list in 
    (match rule with
    | LeftConvRule   -> rule_template legal_con_leftv_helper  conclusion hypothesis_list
    | RightConvRule  -> rule_template legal_con_rightv_helper conclusion hypothesis_list 
    | AbsRule        -> rule_template legal_abs_helper        conclusion hypothesis_list 
    | AppRule        -> legal_app                             conclusion hypothesis_list 
    | LeftAppRule    -> rule_template legal_left_app_helper   conclusion hypothesis_list 
    | RightAppRule   -> rule_template legal_right_app_helper  conclusion hypothesis_list
    | TransitionRule -> legal_transition                      conclusion hypothesis_list
    | UnknownRule    -> UnNamedError("Invalid Rule")
    )
  )


(* utils *)

let str_2_rule rule_str = 
  match rule_str with
  | "LeftConvRule"   -> LeftConvRule
  | "RightConvRule"  -> RightConvRule
  | "AbsRule"        -> AbsRule
  | "AppRule"        -> AppRule
  | "LeftAppRule"    -> LeftAppRule
  | "RightAppRule"   -> RightAppRule
  | "TransitionRule" -> TransitionRule
  | _                -> UnknownRule

let print_rule rule = print_endline
(match rule with
  | LeftConvRule   -> "Left Alpha Conv"
  | RightConvRule  -> "Right Alpha Conv"
  | AbsRule        -> "Absthypo_rightction"
  | AppRule        -> "Application on both side"
  | LeftAppRule    -> "Left Application"
  | RightAppRule   -> "Right Application"
  | TransitionRule -> "Transition"
  | UnknownRule    -> "Invalid Rule"
)

let print_error error = print_endline
(match error with
  | NoError -> "Correct" 
  | NotImplemented -> "Not implemented bhypo_rightnch"
  | NotParsedError -> "Parse error" (* how to raise? *)
  | UndefinedError -> "Undefined error"
  | NumOfHypoError -> "Wrong number of input hypothesis"
  | WrongRule -> "Rule is wrong"

  | LeftConvRightNotSame -> "Right part is not the same"
  | RightConvLeftNotSame -> "Left part is not the same"
  | AlphaConvWrong -> "Illegal alpha conversion"
  | AlphaConvNotChange -> "You don't change anything"
  | AlphaConvChangeTooMany -> "Only one veriable change in one step"

  | LambdaTermNotSame  -> "Can't do absthypo_rightction: lambda not same"
  | AbsRewriteWrong -> "Expression rewrite is wrong"

  | LeftAppRightNotSame -> "Right part not the same"
  | RightAppLeftNotSame -> "Left part not the same"
  | AppLeftRewriteWrong -> "Rewrite error"
  | AppRightRewriteWrong -> "Rewrite error"

  | TransitionRewriteError -> "Transition rewrite wrong"
 
  | UnNamedError str -> str
)
