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
	(* Diamond phi = Neg (Boxe (Neg phi)) *)

type prop = 
(* Propriété de base de la relation *)
	| Reflexive
	| Transitive
	| Symmetric
	| Euclidian
	| Serial

type logic = prop list

type axiom = 
	{v : ident list; 
	(* variables in f, universal quantified *)
	(* TODO : Pas sûr de la représentation : les axiomes semblent canoniques *)
	 f : formula}

type axioms = axiom list

(*--------------------------------------------------------*)
(*      Exemples de logiques & d'axiomes canoniques       *)
(*--------------------------------------------------------*)

(* TODO TODO TODO TODO 

let (k : logic ) = []
let (d : logic ) = [Serial]
let (t : logic ) = [Reflexive]
let (s4 : logic) = [Reflexive; Transitive]
let (s5 : logic) = [Reflexive; Euclidian]

let (n : axiom) = 
	(* TODO : Je suis vraiment pas sûr ... *)
	{v = ["p"];
	 f = (Impl ((Atom "p"),(Boxe (Atom "p"))))}

let (ax_k : axiom) = 
	{v = ["p";"q"];
	 f = Impl( 
	 		(Boxe (Impl(
					(Atom "p"),
					(Atom "q")
				  ))
			),
			Impl(
				Boxe (Atom "p"),
				Boxe (Atom "q")
			))
		}

let (ax_t : axiom) = 
	{v = ["p"];
	 f = Impl(
	 		Boxe (Atom "p"),
			Atom "p"
		 )}

let (ax_4 : axiom) = 
	{v = ["p"];
	 f = Impl(
	 		Boxe (Atom "p"),
			Boxe (Boxe (Atom "p"))
		 )}
*)
