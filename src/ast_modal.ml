(*########################################################*)
(*         Ast pour les formules de logique modale        *)
(*########################################################*)

(*--------------------------------------------------------*)
(*     AST en lui même pour les forumules ..              *)
(*--------------------------------------------------------*)

type ident = string

type formula = 
	| Atom of ident
	| Not of formula
	| Conj of (formula*formula)
	| Dij of (formula*formula)
	| Impl of (formula*formula)
	| Boxe of formula
	| Diamond of formula
