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

let rec isNegation f1 f2 =
match f1,f2 with
| True, False | False, True -> true
| Atom x1, Not (Atom x2) | Not (Atom x1), Atom x2 -> x1 = x2
| Not f1aux, Not f2aux  -> isNegation f1aux f2aux
| Conj (f1g,f1d), Conj (f2g,f2d)
	| Dij (f1g,f1d), Dij (f2g,f2d)
	| Impl (f1g,f1d), Impl (f2g,f2d) ->
	((isNegation f1g f2g) && (isNegation f1d f2d))
	||
	((isNegation f1g f2d) && (isNegation f1d f2g))
| Boxe f1aux, Diamond f2aux | Diamond f1aux, Boxe f2aux -> isNegation f1aux f2aux
| _ -> false

(** We assume that formulas are already in NNF *)
let rec simplify = function
| Not True -> False
| Not False -> True
| Conj (f1, f2) ->
begin
	let f1aux = simplify f1
	and f2aux = simplify f2
	in
	if isNegation f1aux f2aux
		then False
		else match f1aux,f2aux with
					| Boxe f1ter, Boxe f2ter -> Boxe (Dij (f1ter,f2ter))
					| _, False | False, _ -> False
					| f1aux, True -> f1aux
					| True, f2aux -> f2aux
					| _ -> Conj (f1,f2)
end
| Dij (f1, f2) ->
begin
	let f1aux = simplify f1
	and f2aux = simplify f2
	in
	if isNegation f1aux f2aux
		then True
		else match f1aux,f2aux with
					| Diamond f1ter, Diamond f2ter -> Diamond (Dij (f1ter,f2ter))
					| _, True | True, _ -> True
					| f1aux, False -> f1aux
					| False, f2aux -> f2aux
					| _ -> Dij (f1,f2)
end
| Boxe faux -> assert false
| Diamond faux -> assert false
| _ -> assert false
