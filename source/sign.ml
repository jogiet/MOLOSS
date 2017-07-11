(*########################################################*)
(*       Signature pour l'interface de l'oracle SMT       *)
(*########################################################*)

module BFO = Ast_fo.BFO

module type Smt = 
sig

	type ans = 
	| UNSAT 
	| SAT of (string*bool) list

	val init : unit -> unit
	val close : unit -> unit
	(* Procedures pour lancer et eteindre z3 *)

	val dec_const : string -> unit
	val dec_assert : BFO.formula -> unit
	val dec_assert_soft : BFO.formula -> int -> unit
	
	val get_ans : unit -> ans

end
























