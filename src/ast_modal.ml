(*########################################################*)
(*         Ast pour les formules de logique modale        *)
(*########################################################*)

(** This module explicits the AST of modal logic formulas *)


type ident = string

    (** This type represents the modal logic formulas *)
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
  | Not f -> getNNF f
  | Conj (f1,f2) -> Dij (prop_neg f1,prop_neg f2)
  | Dij (f1,f2) -> Conj (prop_neg f1,prop_neg f2)
  | Impl (f1,f2) -> Conj (getNNF f1,prop_neg f2)
  | Boxe f -> Diamond (prop_neg f)
  | Diamond f -> Boxe (prop_neg f)
  | True -> False
  | False -> True

 (** Returns the negativ-normal form of the formula *)
and getNNF = function
  | Atom p -> Atom p
  | Not (Atom p) -> Not (Atom p)
  | Not f -> prop_neg f
  | Conj (f1, f2) -> Conj (getNNF f1, getNNF f2)
  | Dij (f1, f2) -> Dij (getNNF f1, getNNF f2)
  | Impl (f1, f2) -> getNNF (Dij (Not f1, f2))
  | Boxe f -> Boxe (getNNF f)
  | Diamond f -> Diamond(getNNF f)
  | True -> True
  | False -> False

    (** Returns the length of the Modal logic formula *)
let rec formLength = function
  | True | False | Atom _ -> 1
  | Not f | Boxe f | Diamond f -> formLength f + 1
  | Conj (f1,f2) | Dij (f1,f2) | Impl (f1,f2) -> (formLength f1) + (formLength f2) + 1

  (** Returns the modal degree, i.e. then number of [] and <> in the
   * formula *)
let rec modDegree = function
  | True | False | Atom _ -> 0
  | Not f -> modDegree f
  | Boxe f | Diamond f -> modDegree f + 1
  | Conj (f1,f2) | Dij (f1,f2) | Impl (f1,f2) -> (modDegree f1) + (modDegree f2)
