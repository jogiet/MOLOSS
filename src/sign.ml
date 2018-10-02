(**
	Module for generic signatures
*)

module BFO = Ast_fo.BFO

(** This is the signature for an interface between decision precedures and
    the SMT solver *)
module type Smt =
sig

  (** The type of the answer the SMT solver Returns :
      - [ UNSAT ] if the formula is unsat
      - [ SAT ] and the ground model satisfaying the formula *)
	type ans =
	| UNSAT
	| SAT of BFO.atom list

    (** Function to launch a solver. Only used for z3*)
 	val init : unit -> unit

		(** Function to shutdown a solver. *)
	val close : unit -> unit

		(** Function to declare a new ground constant *)
	val dec_const : BFO.atom -> unit

		(** Function to declare a new assertion *)
	val dec_assert : BFO.formula -> unit

		(** Function to declare a new soft-assertion *)
	val dec_assert_soft : BFO.formula -> int -> unit

		(** function to ask the solver for an answer *)
	val get_ans : unit -> ans

end

module type Simplify =
sig

  val simplify : Ast_modal.formula -> Ast_modal.formula

end

module type Decide =
sig

  (** Type for a configuration. Should contains :
      - the worlds
      - the environnement, i.e relation between FO formula and ground formulas
      - set \Theta_something

  *)
  type config

  val new_config : unit -> config

  type model = (BFO.atom) list

  (** This function encode the instanciation procedures.
      It returnes nothing since when a possible instanciation is found,
      it raises an exception.

   *)
  val decide : config -> model -> unit

  val init : config -> Ast_fo.FO.formula list -> Ast_fo.BFO.formula list * BFO.atom list
  (** This function  returns :
      - the list of the boxed formula corresponding to the FO formula
      - the variables in the boxed formula

      and add the formula in the config by side effect.   *)

  exception Found of (BFO.atom list*BFO.formula)
  (** When a decision prcedures is applied, it raises an exception with :
      - the new boxed atoms
      - the new formula
  *)
  exception SoftFound of
      (BFO.atom list*
       BFO.formula*
       BFO.atom list*
       BFO.formula*
       int)
      (** When \exists_soft applies, it raises an exception with :
          - a list of axioms for the new formula,
          - a new FO formula,
          - a list of atoms for the "soft" formula,
          - the soft formula and its weight.
      *)

  val print_model : config -> model -> unit

  val printDecVar : BFO.atom -> config -> unit

  val printAssert : Ast_fo.BFO.formula -> config -> unit

  val printAssertSoft : Ast_fo.BFO.formula -> config -> unit

end

module type DecideArg =
sig
  (** Basically : this argument is the argument in the command line. it contains :
    - the axioms : -S, -K, -4, -5 *)
		val argument : Ast_modal.axiom list
end


   (** This is the signature for a solver module *)
module type Solveur =
sig

  (** You give a formula and
      this function returns you the satifiability of this formula.

      This function should what are the frame axioms applied.
  *)
	val solve : Ast_modal.formula -> bool

end
