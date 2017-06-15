(*########################################################*)
(*       Essai de signature pour un système logique       *)
(*########################################################*)


(*--------------------------------------------------------*)
(*               Modules des ast FO et BFO                *) 
(*--------------------------------------------------------*)
module type Ms = sig

	type formula

end

module type FOs = sig

	type formula
	(*
	modif possible, faire apparaitre la quantification dans l'AST ...
	on vire quantified of quantifiers et on met forall et Exist
	=> code + lisible ...

	*)



	val equalmodvquant : formula -> formula -> bool 

	(* 
	les types t & fonctions compare ne sont pas spécifiées,
	il faut le construire si besoin
	*)

end


module type BFOs = sig
	
	type atom
	type value
	type formula 


end


(*--------------------------------------------------------*)
(*                 Modules de transitions                 *)
(*--------------------------------------------------------*)

module type Truc = sig
	
	

	module M : Ms
	module FO : FOs
	module BFO : BFOs

	type env (* question : prendre des structures persistantes ou pas, en
	effet, pas de backtrack ... *)
	
	val abs : env -> FO.formula -> BFO.formula*env*(BFO.atom list)
	(* 
	renvoie :
		-> la nouvelle formule, 
		-> le nouvel environnement (si structure perssitante) et 
		-> les nouveaux atomes introduits 
	*)

	val conc : env -> BFO.formula -> FO.formula

	type modele = (BFO.atom*BFO.value) list 

	type config (* idem que pôur env *)

	val make_config : unit -> config
	(* Normalement, une config contient un enregistrement *)

	val env_of_config : config -> env
	(* Pour le foncteur *)

	type decided = 
		| Sat
		| ToContinue of config*(BFO.atom list)*(BFO.formula list)

	val decide : config -> modele -> decided
	(* On peut essayer de faire le max de écision *)

	(*   Partie communication avec le solveur SMT   *)

	type in_chan (* channel pour recevoir du solveur *)
	type out_chan (* channel pour l'envoi *)

	type answer = 
		| UNSAT
		| SAT of modele

	val add_const : out_chan -> BFO.atom -> unit

	val add_formula : out_chan -> BFO.formula -> unit

	val check_sat : unit -> answer
	(*
	Fonction problématique : 
	-> le retour dépend ud solveur et de la logique ...
	solution : Parser le modèle brutalement et le renvoyer dans un syst
	logique assez "grand" pour qu ça marche dans la restriction
	considérée (ie : avec forall).
	supposition : Si on restreint le système (logique, d'assertions...)
	alors, le solveur restreint le modèle au coeur ...
	*)

	val init : unit -> (in_chan*out_chan)
	(* 
	se charge de lancer la coomunication avec le solveur
	et d'initier le système logique, e.g. : (set-logic)
	*)

end

module S (L : Truc) = struct

	module FO = L.FO
	module BFO = L.BFO

	let solve f = assert false
	(*
	PSEUDO-CODE : 
	--> Init:
	on init la communication avec le solveur
	on prend une config videa : c
	on la remplit à partir de abs (env_of_config c) f
	on ajoute au système les atomes (eq de varaiables) 
		et la formule
	tant que vrai faire
		interroger le solveur :
		| UNSAT -> raise qqch 
		| SAT m -> 
			interroger la proc de décision sur la config et m:
			| Sat ->  raise qqc2
			| ToContinue _ ->
			On enrichit le système : 
			on ajoutes les atomes,
			on ajoutes les asserts,
	FinTantque
	gereexcn : 
	qqch -> "la formule est insatisfiabe"
	qqch2 -> "la formùule est satisfiable ..."
	*)

end




(*--------------------------------------------------------*)






































