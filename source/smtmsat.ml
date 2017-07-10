(*###################################################################*)
(*                     Résolution par minisat                        *)
(*###################################################################*)

(*
/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
Le module minisat n'a pas de fonction clear, il faut donc utilser qu'une
fois par appel sinon on a des résultats incohérents  
*)

open Msat

module SMTmsat : Sign.Smt = 
struct

module Sat = Msat.Sat.Make()
module E = Msat.Sat.Expr
module F = Msat.Tseitin.Make (E)
module H = Hashtbl
module BFO = Ast_fo.BFO

let printbug s = 
	print_string s;
	flush_all ()

type ans = 
	| UNSAT
	| SAT of (string*bool) list


let stoa = H.create 42

let init () = H.reset stoa
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
	printbug "assert_soft pas implem'\n"; exit 0

let get_ans () = 
	match 
	Sat.solve () 
	with
	| Sat.Unsat _ -> UNSAT
	| Sat.Sat state -> 
	let model = ref [] in
	let aux s a = 
		try 
			model := (s,state.Msat.Solver_intf.eval a)::(!model)
		with _ ->
			model := (s,false)::(!model)
	in begin
		(try
		H.iter aux stoa
		with _ -> begin printbug "H.iter chiale\n"; exit 0 ; end);
		try 
		SAT !model
		with _ -> begin printbug "SAT!model chiale\n"; exit 0; end;
	end

end
