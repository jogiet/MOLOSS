(*###################################################################*)
(*                     Interface pour minisat                        *)
(*###################################################################*)

(*
/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
donc le module de résolution est un foncteur, ce qui permet de le définir
plusieurs fois pour faire des tests en cascade
*)

(**
   A Dummy module in order to clean the solver
   between two formulas (used for tests).
*)
module type Idiot =
sig
	val truc : int
end

open Msat

module SMTmsat (Dummy : Idiot) : Sign.Smt =
struct

module Sat = Msat.Sat.Make()
module E = Msat.Sat.Expr
module F = Msat.Tseitin.Make (E)
module H = Hashtbl
module Q = Queue
module BFO = Ast_fo.BFO

    (** A printing function for debug *)
let printbug s =
	print_string s;
	flush_all ()

(** This FIFO contains the guards for the assert-soft formulas
    that are still valid *)
let assump = Q.create ()

(** This FIFO contains the guards for the assert-soft formulas
    that are not valid *)
let negassump = Q.create ()

type ans =
	| UNSAT
	| SAT of (BFO.atom) list

 		(** This hashtbl maps the BFO axiom to the mSAT axioms *)
let stoa = H.create 42

let init () =
begin
	Q.clear assump;
	H.reset stoa;
end

let close () = H.reset stoa

let dec_const s =
try
	let atom = E.fresh ()
	in begin
		H.add stoa s atom;
	end
with _ -> printbug "dec_const chiale \n"; exit 0

let rec bfo_to_msat f =
try
match f with
| BFO.Atom s -> F.make_atom (H.find stoa s)
| BFO.Not p -> F.make_not (bfo_to_msat p)
| BFO.Conj (f1,f2) -> F.make_and [bfo_to_msat f1; bfo_to_msat f2]
| BFO.Dij (f1,f2) -> F.make_or [bfo_to_msat f1; bfo_to_msat f2]
with _ -> printbug "bfo_tomsat chiale \n"; exit 0



let dec_assert f =
try
	Sat.assume (F.make_cnf (bfo_to_msat f))
with _ -> printbug "dec_assert chiale \n"; exit 0

let dec_assert_soft f w =

	let g = E.fresh () in
	let cls = F.make_cnf (F.make_or [F.make_not (F.make_atom g);
									(bfo_to_msat f)])
	and atomg = match F.make_cnf (F.make_atom g) with
				| [[x]] -> x
				| _ -> assert false
	in begin
		Q.push atomg assump;
		Sat.assume cls;
	end

    (** Returns the list of asserted guards *)
let get_assum () =
	let res = ref [] in
	let aux at = res := at::(!res)
	in begin
		Q.iter aux assump;
		!res;
	end


let rec get_ans () =
	match Sat.solve ~assumptions:(get_assum ()) () with
	| Sat.Unsat _ ->
		if Q.is_empty assump then
			UNSAT
		else
		begin
			Q.pop assump |> ignore;
			get_ans ();
		end
	| Sat.Sat state ->
	let model = ref [] in
	let aux s a =
    if try state.Msat.Solver_intf.eval a with _ -> false then
      model := s::(!model)
	in begin
		H.iter aux stoa;
		SAT !model;
	end

end
