(*########################################################*)
(*         Ast pour les formules de logique modale        *)
(*########################################################*)



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

  (** This function propagate a negation in a modal formula.
     Helps to compute the NNF of the formula *)
let rec prop_neg = function
  | Atom p -> Not (Atom p)
  | Not f -> f (* Tertium non datur *)
  | Conj (f1,f2) -> Dij (prop_neg f1,prop_neg f2)
  | Dij (f1,f2) -> Conj (prop_neg f1,prop_neg f2)
  | Impl (f1,f2) -> Conj (f1,prop_neg f2)
  | Boxe f -> Diamond (prop_neg f)
  | Diamond f -> Boxe (prop_neg f)
  | True -> False
  | False -> True

let rec formLength = function
    (** Returns the length of the Modal logic formula *)
  | True | False | Atom _ -> 1
  | Not f | Boxe f | Diamond f -> formLength f + 1
  | Conj (f1,f2) | Dij (f1,f2) | Impl (f1,f2) -> (formLength f1) + (formLength f2) + 1

let rec modDegree = function
    (** Returns the modal degree*)
  | True | False | Atom _ -> 0
  | Not f -> modDegree f
  | Boxe f | Diamond f -> modDegree f + 1
  | Conj (f1,f2) | Dij (f1,f2) | Impl (f1,f2) -> (modDegree f1) + (modDegree f2)
