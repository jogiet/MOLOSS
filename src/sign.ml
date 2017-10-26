(*
	Module for generic signatures
*)

module BFO = Ast_fo.BFO

(** This is the signature for an interface between decision precedures and
    the SMT solver *)
module type Smt =
sig

  (** The type of the answer the SMT solver Returns :
      UNSAT if the formula is unsat
      or SAT and the ground model satisfaying the formula *)
	type ans =
	| UNSAT
	| SAT of (string*bool) list

    (** Function to launch a solver. Only used for z3*)
 	val init : unit -> unit

		(** Function to shutdown a solver. *)
	val close : unit -> unit

		(** Function to declare a new ground constant *)
	val dec_const : string -> unit

		(** Function to declare a new assertion *)
	val dec_assert : BFO.formula -> unit

		(** Function to declare a new soft-assertion *)
	val dec_assert_soft : BFO.formula -> int -> unit

		(** function to ask the solver for an answer *)
	val get_ans : unit -> ans

end

   (** This is the signature for a solver module *)
module type Solveur =
sig

  (** You give a formula and the list of axioms,
      this function returns you the satifiability of this formula *)
	val solve : Ast_fo.FO.formula -> string list -> bool

end
