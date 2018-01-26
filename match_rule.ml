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
 * before_list: have only one element, i.e. "left ~a~ right" string
 * after_list: have one or two elements, each element is a "left ~a~ right" string
 * rules:
   * a_conv: 1 -> 1
   * abs: 1 -> 1 (lambda term should be equal)
   * app: 1 -> 2
 *)

type error = 
  | NoError
  | NotImplemented
  | NotParsedError (* how to raise? *)
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

(* judge whether two lists are the same *)
let rec same_list l1 l2 = match (l1, l2) with
  | ([], []) -> true
  | (x::xs, y::ys) -> x = y && same_list xs ys
  | (_, _) -> false

(* judge whether two binding relation lists are the same *)
let rec same_bind_list l1 l2 = match (l1, l2) with
  | ([], []) -> true 
  | (Bind(id_1, bounded_1, str_1)::xs, Bind(id_2, bounded_2, str_2)::ys) -> 
      id_1 = id_2 && same_list bounded_1 bounded_2 && same_bind_list xs ys
  | (Free(id_1, s1)::xs, Free(id_2, s2)::ys) ->
      id_1 = id_2 && s1 = s2 && same_bind_list xs ys
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
let legal_left_conv before_list after_list = 
  match (before_list, after_list) with
  | (before::[], after::[]) ->
      (match (parse_exp before, parse_exp after) with
      | ((lb, rb), (la, ra)) ->
          if not (can_alpha_conv lb) then WrongRule else 
          if not (same_lambda rb ra) then LeftConvRightNotSame else 
          is_alpha_conversion lb la
      | (_, _) -> NotImplemented (* One *)
      )
  | _ -> NotImplemented (* num uplineterms *)

let legal_right_conv before_list after_list = 
  match (before_list, after_list) with
  | (before::[], after::[]) ->
      (match (parse_exp before, parse_exp after) with
      | ((lb, rb), (la, ra)) ->
          if not (can_alpha_conv rb) then WrongRule else 
          if not (same_lambda lb la) then RightConvLeftNotSame else 
          is_alpha_conversion rb ra
      | (_, _) -> NotImplemented (* One *)
      )
  | _ -> NotImplemented (* num uplineterms *)


let legal_abs_helper lb rb la ra = 
  match (lb, rb) with
  | (AbsLam(s_lb, e_lb), AbsLam(s_rb, e_rb)) ->
      if not (s_lb = s_rb) then LambdaTermNotSame 
      else if not (same_lambda e_lb la) then AbsRewriteWrong 
      else if not (same_lambda e_rb ra) then AbsRewriteWrong
      else NoError
  | _ -> WrongRule 

let legal_abs before_list after_list = 
  match (before_list, after_list) with
  | (before::[], after::[]) ->
      (match (parse_exp before, parse_exp after) with
      | ((lb, rb), (la, ra)) -> 
          if not (can_abs lb) then WrongRule else 
          legal_abs_helper lb rb la ra 
      | (_, _) -> NotImplemented (* One *)
      )
  | _ -> NotImplemented (* num uplineterms *)


let legal_app_helper lb rb la1 ra1 la2 ra2 = 
  match (lb, rb) with
  | (AppLam(e1_lb, e2_lb), AppLam(e1_rb, e2_rb)) ->
      if not (same_lambda e1_lb la1) then AppLeftRewriteWrong else 
      if not (same_lambda e2_lb la2) then AppLeftRewriteWrong else 
      if not (same_lambda e1_rb ra1) then AppRightRewriteWrong else 
      if not (same_lambda e2_rb ra2) then AppRightRewriteWrong else 
      NoError 
  | _ -> WrongRule 

let legal_app before_list after_list = 
  match (before_list, after_list) with
  | (before::[], after_1::after_2::[]) ->
      (match (parse_exp before, parse_exp after_1, parse_exp after_2) with
      | ((lb, rb), (la1, ra1), (la2, ra2)) -> 
          legal_app_helper lb rb la1 ra1 la2 ra2
      | (_, _, _) -> NotImplemented (* One *)
      )
  | _ -> NotImplemented (* num uplineterms *)


let legal_left_app_helper lb rb la ra = 
  match (lb, rb) with
  | (AppLam(e1_lb, e2_lb), AppLam(e1_rb, e2_rb)) ->
      if not (same_lambda e1_lb la) then AppLeftRewriteWrong else 
      if not (same_lambda e1_rb ra) then AppLeftRewriteWrong else 
      if not (same_lambda e2_lb e2_rb) then LeftAppRightNotSame else
      NoError 
  | _ -> WrongRule 

let legal_left_app before_list after_list = 
  match (before_list, after_list) with
  | (before::[], after::[]) ->
      (match (parse_exp before, parse_exp after) with
      | ((lb, rb), (la, ra)) -> 
          legal_left_app_helper lb rb la ra 
      | (_, _) -> NotImplemented (* One *)
      )
  | _ -> NotImplemented (* num uplineterms *)


let legal_right_app_helper lb rb la ra = 
  match (lb, rb) with
  | (AppLam(e1_lb, e2_lb), AppLam(e1_rb, e2_rb)) ->
      if not (same_lambda e2_lb la) then AppRightRewriteWrong else 
      if not (same_lambda e2_rb ra) then AppRightRewriteWrong else 
      if not (same_lambda e1_lb e1_rb) then RightAppLeftNotSame else 
      NoError 
  | _ -> WrongRule 

let legal_right_app before_list after_list = 
  match (before_list, after_list) with
  | (before::[], after::[]) ->
      (match (parse_exp before, parse_exp after) with
      | ((lb, rb), (la, ra)) -> 
          legal_right_app_helper lb rb la ra 
      | (_, _) -> NotImplemented (* One *)
      )
  | _ -> NotImplemented (* num uplineterms *)


let legal_onestep rule before_list after_list = 
  match rule with
  | LeftConvRule  -> legal_left_conv  before_list after_list
  | RightConvRule -> legal_right_conv before_list after_list 
  | AbsRule       -> legal_abs        before_list after_list 
  | AppRule       -> legal_app        before_list after_list 
  | LeftAppRule   -> legal_left_app   before_list after_list 
  | RightAppRule  -> legal_right_app  before_list after_list 
  | UnknownRule   -> raise(Failure("Invalid Rule")) 

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
  | AbsRule -> "Abstraction"
  | AppRule -> "Application on both side"
  | LeftAppRule -> "Left Application"
  | RightAppRule -> "Right Application"
  | UnknownRule -> "Unknown"
)

let print_error error = print_endline
(match error with
  | NoError -> "Correct" 
  | NotImplemented -> "Not implemented branch"
  | NotParsedError -> "Parse error" (* how to raise? *)
  | UndefinedError -> "Undefined error"
  | WrongRule -> "Rule is wrong"

  | LeftConvRightNotSame -> "Right part is not the same"
  | RightConvLeftNotSame -> "Left part is not the same"
  | AlphaConvWrong -> "Illegal alpha conversion"
  | AlphaConvNotChange -> "You don't change anything"
  | AlphaConvChangeTooMany -> "Only one veriable change in one step"

  | LambdaTermNotSame  -> "Can't do abstraction: lambda not same"
  | AbsRewriteWrong -> "Expression rewrite is wrong"

  | LeftAppRightNotSame -> "Right part not the same"
  | RightAppLeftNotSame -> "Left part not the same"
  | AppLeftRewriteWrong -> "Rewrite error"
  | AppRightRewriteWrong -> "Rewrite error"
 
  | UnNamedError str -> str
)
