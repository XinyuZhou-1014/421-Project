open Lambda;;
open Lambda_parse;;

type rule =
  | LeftConvRule
  | RightConvRule
  | AbsRule
  | AppRule
  | LeftAppRule
  | RightAppRule
  | UnknownRule
(* 
 * rule: type of Rule
 * conclusion: have only one element, i.e. "left ~a~ right" string
 * hypothesis_list: have one or two elements, each element is a "left ~a~ right" string
 * rules:
   * a_conv: 1 -> 1
   * abs: 1 -> 1 (lambda term should be equal)
   * app: 1 -> 2
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
  (* conv not good, can't abs, can't app, 
   * abs lambda not equal, abs rewrite wrong, 
   * left app right not same, 
   * right app left not same,
   * app left rewrite wrong, app right rewrite wrong*)

let parse_exp str = 
  let expr = Lambda_parse.input Lambda_lex.token
             (Lexing.from_string str)
  in match expr with
  | One lam -> raise(Failure "should have two here")
  | Two (left, right) -> (left, right)

let can_alpha_conv lam = true

let can_abs lam = match lam with AbsLam _ -> true | _ -> false

let can_app lam = match lam with AppLam _ -> true | _ -> false

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
let legal_left_conv conclusion hypothesis_list = 
  match hypothesis_list with
  | hypothesis::[] ->
      (match (conclusion, hypothesis) with
      | ((left_con, right_con), (left_hypo, right_hypo)) ->
          if not (can_alpha_conv left_con) then WrongRule else 
          if not (same_lambda right_con right_hypo) then LeftConvRightNotSame else 
          is_alpha_conversion left_con left_hypo
      | (_, _) -> NotImplemented (* One *)
      )
  | _ -> NumOfHypoError

let legal_right_conv conclusion hypothesis_list = 
  match hypothesis_list with
  | hypothesis::[] ->
      (match (conclusion, hypothesis) with
      | ((left_con, right_con), (left_hypo, right_hypo)) ->
          if not (can_alpha_conv right_con) then WrongRule else 
          if not (same_lambda left_con left_hypo) then RightConvLeftNotSame else 
          is_alpha_conversion right_con right_hypo
      | (_, _) -> NotImplemented (* One *)
      )
  | _ -> NumOfHypoError


let legal_abs_helper left_con right_con left_hypo right_hypo = 
  match (left_con, right_con) with
  | (AbsLam(s_left_con, e_left_con), AbsLam(s_right_con, e_right_con)) ->
      if not (s_left_con = s_right_con) then LambdaTermNotSame 
      else if not (same_lambda e_left_con left_hypo) then AbsRewriteWrong 
      else if not (same_lambda e_right_con right_hypo) then AbsRewriteWrong
      else NoError
  | _ -> WrongRule 

let legal_abs conclusion hypothesis_list = 
  match hypothesis_list with
  | hypothesis::[] ->
      (match (conclusion, hypothesis) with
      | ((left_con, right_con), (left_hypo, right_hypo)) -> 
          if not (can_abs left_con) then WrongRule else 
          legal_abs_helper left_con right_con left_hypo right_hypo 
      | (_, _) -> NotImplemented (* One *)
      )
  | _ -> NumOfHypoError


let legal_app_helper left_con right_con left_hypo_1 right_hypo_1 left_hypo_2 right_hypo_2 = 
  match (left_con, right_con) with
  | (AppLam(e1_left_con, e2_left_con), AppLam(e1_right_con, e2_right_con)) ->
      if not (same_lambda e1_left_con left_hypo_1) then AppLeftRewriteWrong else 
      if not (same_lambda e2_left_con left_hypo_2) then AppLeftRewriteWrong else 
      if not (same_lambda e1_right_con right_hypo_1) then AppRightRewriteWrong else 
      if not (same_lambda e2_right_con right_hypo_2) then AppRightRewriteWrong else 
      NoError 
  | _ -> WrongRule 

let legal_app conclusion hypothesis_list = 
  match hypothesis_list with
  | hypothesis_1::hypothesis_2::[] ->
      (match (conclusion, hypothesis_1, hypothesis_2) 
      with
      | ((left_con, right_con), (left_hypo_1, right_hypo_1), (left_hypo_2, right_hypo_2)) -> 
          legal_app_helper left_con right_con left_hypo_1 right_hypo_1 left_hypo_2 right_hypo_2
      | (_, _, _) -> NotImplemented (* One *)
      )
  | _ -> NumOfHypoError


let legal_left_app_helper left_con right_con left_hypo right_hypo = 
  match (left_con, right_con) with
  | (AppLam(e1_left_con, e2_left_con), AppLam(e1_right_con, e2_right_con)) ->
      if not (same_lambda e1_left_con left_hypo) then AppLeftRewriteWrong else 
      if not (same_lambda e1_right_con right_hypo) then AppLeftRewriteWrong else 
      if not (same_lambda e2_left_con e2_right_con) then LeftAppRightNotSame else
      NoError 
  | _ -> WrongRule 

let legal_left_app conclusion hypothesis_list = 
  match hypothesis_list with
  | hypothesis::[] ->
      (match (conclusion, hypothesis) with
      | ((left_con, right_con), (left_hypo, right_hypo)) -> 
          legal_left_app_helper left_con right_con left_hypo right_hypo 
      | (_, _) -> NotImplemented (* One *)
      )
  | _ -> NumOfHypoError


let legal_right_app_helper left_con right_con left_hypo right_hypo = 
  match (left_con, right_con) with
  | (AppLam(e1_left_con, e2_left_con), AppLam(e1_right_con, e2_right_con)) ->
      if not (same_lambda e2_left_con left_hypo) then AppRightRewriteWrong else 
      if not (same_lambda e2_right_con right_hypo) then AppRightRewriteWrong else 
      if not (same_lambda e1_left_con e1_right_con) then RightAppLeftNotSame else 
      NoError 
  | _ -> WrongRule 

let legal_right_app conclusion hypothesis_list = 
  match hypothesis_list with
  | hypothesis::[] ->
      (match (conclusion, hypothesis) with
      | ((left_con, right_con), (left_hypo, right_hypo)) -> 
          legal_right_app_helper left_con right_con left_hypo right_hypo 
      | (_, _) -> NotImplemented (* One *)
      )
  | _ -> NumOfHypoError


let legal_onestep onestep_input =
  (match onestep_input with OneStepInput(con_str, rule, hypo_str_list) -> 
    let conclusion = parse_exp con_str in 
    let hypothesis_list = List.map parse_exp hypo_str_list in 
    (match rule with
    | LeftConvRule  -> legal_left_conv  conclusion hypothesis_list
    | RightConvRule -> legal_right_conv conclusion hypothesis_list 
    | AbsRule       -> legal_abs        conclusion hypothesis_list 
    | AppRule       -> legal_app        conclusion hypothesis_list 
    | LeftAppRule   -> legal_left_app   conclusion hypothesis_list 
    | RightAppRule  -> legal_right_app  conclusion hypothesis_list 
    | UnknownRule   -> UnNamedError("Invalid Rule")
    )
  )

(* utils *)
let str_2_rule rule_str = 
  match rule_str with
  | "LeftConvRule"  -> LeftConvRule
  | "RightConvRule" -> RightConvRule
  | "AbsRule"       -> AbsRule
  | "AppRule"       -> AppRule
  | "LeftAppRule"   -> LeftAppRule
  | "RightAppRule"  -> RightAppRule
  | _ -> UnknownRule

let print_rule rule = print_endline
(match rule with
  | LeftConvRule -> "Left Alpha Conv"
  | RightConvRule -> "Right Alpha Conv"
  | AbsRule -> "Abstright_hypoction"
  | AppRule -> "Application on both side"
  | LeftAppRule -> "Left Application"
  | RightAppRule -> "Right Application"
  | UnknownRule -> "Invalid Rule"
)

let print_error error = print_endline
(match error with
  | NoError -> "Correct" 
  | NotImplemented -> "Not implemented bright_hyponch"
  | NotParsedError -> "Parse error" (* how to raise? *)
  | UndefinedError -> "Undefined error"
  | NumOfHypoError -> "Wrong number of input hypothesis"
  | WrongRule -> "Rule is wrong"

  | LeftConvRightNotSame -> "Right part is not the same"
  | RightConvLeftNotSame -> "Left part is not the same"
  | AlphaConvWrong -> "Illegal alpha conversion"
  | AlphaConvNotChange -> "You don't change anything"
  | AlphaConvChangeTooMany -> "Only one veriable change in one step"

  | LambdaTermNotSame  -> "Can't do abstright_hypoction: lambda not same"
  | AbsRewriteWrong -> "Expression rewrite is wrong"

  | LeftAppRightNotSame -> "Right part not the same"
  | RightAppLeftNotSame -> "Left part not the same"
  | AppLeftRewriteWrong -> "Rewrite error"
  | AppRightRewriteWrong -> "Rewrite error"
 
  | UnNamedError str -> str
)
