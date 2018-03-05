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
  | TerminateRule
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
  | OneStepInput of string * string * string list (* conclusion, rule_str, hypothesis list *)

type error = 
  | NoError
  | NotImplemented
  | LexingError
  | ParseError
  | NumOfHypoError
  | UndefinedError of exn
  | WrongRule
  | UnNamedError of string
  (* alpha conversion *)
  | AlphaConvTermWrong
  | LeftConvRightNotSame
  | RightConvLeftNotSame
  | AlphaConvWrong
  | AlphaConvVariableWrong
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


(* judge whether two lists are the same *)
let rec same_lists l1 l2 = 
  match (l1, l2) with
  | ([], []) -> true 
  | (x::xs, y::ys) -> if x = y then same_lists xs ys else false
  | _ -> false

(* judge whether two typed lambda are the same *)
let rec same_lambda e1 e2 = match (e1, e2) with
  | (VarLam s1, VarLam s2) -> s1 = s2
  | (AbsLam (s1, r1), AbsLam (s2, r2)) ->
      s1 = s2 && same_lambda r1 r2
  | (AppLam (l1, r1), AppLam(l2, r2)) ->
      same_lambda l1 l2 && same_lambda r1 r2
  | (_, _) -> false


(* Check alpha conversion *)
(* First check the outermost is AbsLam, 
   then check only the name of the outermost binding variable changes *)

(* all but the first term *)
let rec alpha_conversion_helper bind_list_1 bind_list_2 = 
  match (bind_list_1, bind_list_2) with 
  | ([], []) -> NoError
  | ((Bind(id_1, bounded_1, str_1)::xs, Bind(id_2, bounded_2, str_2)::ys)) ->
    if not (id_1 = id_2 && bounded_1 = bounded_2) then AlphaConvWrong else
    if not (str_1 = str_2) then AlphaConvVariableWrong else
    alpha_conversion_helper xs ys
  | (Free(id_1, str_1)::xs, Free(id_2, str_2)::ys) ->
    if not (id_1 = id_2 && str_1 = str_2) then AlphaConvWrong else
    alpha_conversion_helper xs ys
  | _ -> AlphaConvWrong 

(* First check abstraction, then deal with first term, then throw to helper *)
let is_alpha_conversion lam1 lam2 = 
  match (lam1, lam2) with
  | (AbsLam(s_, _), AbsLam(_, _)) -> 
    (match ((Lambda.get_binding_relation lam1),  
           (Lambda.get_binding_relation lam2))
     with 
     | ([], []) -> UnNamedError "System error: nothing in bind list." 
     | (Bind(id_1, bounded_1, str_1)::xs, Bind(id_2, bounded_2, str_2)::ys) -> 
       if not (id_1 = 1 && id_2 = 1) then AlphaConvVariableWrong else
       if not (same_lists bounded_1 bounded_2) then AlphaConvWrong else
       alpha_conversion_helper xs ys
     | _ -> AlphaConvWrong
    )
  | _ -> AlphaConvTermWrong


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

let legal_terminate conclusion hypothesis_list = 
  match hypothesis_list with 
  | [] -> 
    (match conclusion with
    | (con_left, con_right) -> 
      if not (same_lambda con_left con_right) then WrongRule else
      NoError
    | (_, _) -> NotImplemented (* One *)
    )
  | _ -> NumOfHypoError

(* correct only if right part unchanged and left part a valid alpha conversion *)
let legal_conv_left_helper con_left con_right hypo_left hypo_right = 
  if not (same_lambda con_right hypo_right) then LeftConvRightNotSame else 
  is_alpha_conversion con_left hypo_left

(* similar as above *)
let legal_conv_right_helper con_left con_right hypo_left hypo_right = 
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
      if not (same_lambda con_left_rand snd_hypo_left)    then AppLeftRewriteWrong else 
      if not (same_lambda con_right_rator fst_hypo_right) then AppRightRewriteWrong else 
      if not (same_lambda con_right_rand snd_hypo_right)  then AppRightRewriteWrong else 
      NoError 
    | _ -> WrongRule 
    )
  | (conlusion, _::_::[]) -> NotImplemented (* One *)
  | _ -> NumOfHypoError

(* check left part rewrite *)
let legal_left_app_helper con_left con_right hypo_left hypo_right = 
  match (con_left, con_right) with
  | (AppLam(con_left_rator, con_left_rand), AppLam(con_right_rator, con_right_rand)) ->
      if not (same_lambda con_left_rator hypo_left)     then AppLeftRewriteWrong else 
      if not (same_lambda con_right_rator hypo_right)   then AppLeftRewriteWrong else 
      if not (same_lambda con_left_rand con_right_rand) then LeftAppRightNotSame else
      NoError 
  | _ -> WrongRule 

(* check right part rewrite *)
let legal_right_app_helper con_left con_right hypo_left hypo_right = 
  match (con_left, con_right) with
  | (AppLam(con_left_rator, con_left_rand), AppLam(con_right_rator, con_right_rand)) ->
      if not (same_lambda con_left_rand hypo_left)        then AppRightRewriteWrong else 
      if not (same_lambda con_right_rand hypo_right)      then AppRightRewriteWrong else 
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


(* utils *)

let str_2_rule rule_str = 
  match rule_str with
  | "TerminateRule"  -> TerminateRule
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
  | TerminateRule  -> "End"
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
  | NotImplemented -> "Not implemented"
  | ParseError -> "Parse error" (* how to raise? *)
  | LexingError -> "Lexing Error"
  | UndefinedError e -> raise e; "Undefined error"
  | NumOfHypoError -> "Wrong number of input hypothesis"
  | WrongRule -> "Rule is wrong"

  | AlphaConvTermWrong -> "Alpha conversion only happens on abstraction terms"
  | LeftConvRightNotSame -> "Right part is not the same"
  | RightConvLeftNotSame -> "Left part is not the same"
  | AlphaConvWrong -> "Illegal alpha conversion"
  | AlphaConvVariableWrong -> "Alpha conversion only on the outermost variable of abstraction"

  | LambdaTermNotSame  -> "Binding variable not same, do alpha conversion first"
  | AbsRewriteWrong -> "Expression rewrite is wrong"

  | LeftAppRightNotSame -> "Right part not the same"
  | RightAppLeftNotSame -> "Left part not the same"
  | AppLeftRewriteWrong -> "Rewrite error"
  | AppRightRewriteWrong -> "Rewrite error"

  | TransitionRewriteError -> "Transition rewrite wrong"
 
  | UnNamedError str -> str
)


(* main function *)
let legal_onestep onestep_input =
  try
    (match onestep_input with OneStepInput(con_str, rule_str, hypo_str_list) -> 
      let rule = str_2_rule rule_str in
      let conclusion = parse_exp con_str in 
      let hypothesis_list = List.map parse_exp hypo_str_list in 
      (match rule with
      | LeftConvRule   -> rule_template legal_conv_left_helper  conclusion hypothesis_list
      | RightConvRule  -> rule_template legal_conv_right_helper conclusion hypothesis_list 
      | AbsRule        -> rule_template legal_abs_helper        conclusion hypothesis_list 
      | AppRule        -> legal_app                             conclusion hypothesis_list 
      | LeftAppRule    -> rule_template legal_left_app_helper   conclusion hypothesis_list 
      | RightAppRule   -> rule_template legal_right_app_helper  conclusion hypothesis_list
      | TransitionRule -> legal_transition                      conclusion hypothesis_list
      | UnknownRule    -> UnNamedError("Invalid Rule")
      )
    )
  with e -> match e with
  | Parsing.Parse_error -> ParseError
  | Failure str -> LexingError
  | _ -> UndefinedError e

