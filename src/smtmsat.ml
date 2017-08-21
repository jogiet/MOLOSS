(*###################################################################*)
(*                     Interface pour minisat                        *)
(*###################################################################*)

(*
/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
donc le module de résolution est un foncteur, ce qui permet de le définir 
plusieurs fois pour faire des tests en cascade
*)

module type Idiot = 
(* On définit un module pour faire les appels  *)
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

let printbug s = 
	print_string s;
	flush_all ()

let assump = Q.create ()
let negassump = Q.create () 

type ans = 
	| UNSAT
	| SAT of (string*bool) list


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
		try 
			model := (s,state.Msat.Solver_intf.eval a)::(!model)
		with _ ->
			model := (s,false)::(!model)
	in begin
		H.iter aux stoa;
		SAT !model;
	end

end
