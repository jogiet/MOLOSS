(*########################################################*)
(*         Ast pour les formules de logique modale        *)
(*########################################################*)

(*--------------------------------------------------------*)
(*     AST en lui même pour les forumules ..              *)
(*--------------------------------------------------------*)

type ident = string

type formula =
	| True | False
	| Atom of int
	| Not of formula
	| Conj of (formula*formula)
	| Dij of (formula*formula)
	| Impl of (formula*formula)
	| Boxe of formula
	| Diamond of formula

let rec formLength = function
    (** Returns the length of th Modal logic formula *)
  | True | False | Atom _ -> 1
  | Not f | Boxe f | Diamond f -> formLength f + 1
  | Conj (f1,f2) | Dij (f1,f2) | Impl (f1,f2) -> (formLength f1) + (formLength f2) + 1

let rec isNegation f1 f2 =
match f1,f2 with
| True, False | False, True -> true
| Atom x1, Not (Atom x2) | Not (Atom x1), Atom x2 -> x1 = x2
| Not f1aux, Not f2aux  -> isNegation f1aux f2aux
| Conj (f1g,f1d), Conj (f2g,f2d)
	| Dij (f1g,f1d), Dij (f2g,f2d) ->
	((isNegation f1g f2g) && (isNegation f1d f2d))
	||
 ((isNegation f1g f2d) && (isNegation f1d f2g))
| Impl (f1g,f1d), Impl (f2g,f2d) -> assert false
| Boxe f1aux, Diamond f2aux | Diamond f1aux, Boxe f2aux -> isNegation f1aux f2aux
| _ -> false

(** We assume that formulas are already in NNF *)
let rec simplify = function
| True -> True
| False -> False
| Atom x -> Atom x
| Not True -> False
| Not False -> True
| Not f -> Not f
| Conj (f1, f2) ->
begin
  match simplify f1, simplify f2 with
  | f1aux, f2aux when isNegation f1aux f2aux -> False
	| Boxe f1ter, Boxe f2ter -> Boxe (Dij (f1ter,f2ter))
	| _, False | False, _ -> False
	| f1aux, True -> f1aux
	| True, f2aux -> f2aux
	| _ -> Conj (f1,f2)
end
| Dij (f1, f2) ->
begin
  match simplify f1, simplify f2 with
  | f1aux, f2aux when isNegation f1aux f2aux -> True
  | Diamond f1ter, Diamond f2ter -> Diamond (Dij (f1ter,f2ter))
  | _, True | True, _ -> True
  | f1aux, False -> f1aux
  | False, f2aux -> f2aux
  | f1aux, f2aux -> Dij (f1aux, f2aux)
end
| Impl (f1, f2) -> simplify (Dij (Not f1, f2))
| Boxe f ->
begin
  match simplify f with
  | True -> True
  | False -> assert false
  | faux -> Boxe faux
end
| Diamond f ->
begin
	match  simplify f with
   | True -> assert false (* TODO : find somrthing clever ...*)
   | False -> False
   | faux -> Diamond faux
end
(* | _ -> assert false *)
