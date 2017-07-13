(*########################################################*)
(*                 Interface pour minisat                 *)
(*########################################################*)

module M = Minisat
module BFO = Ast_fo.BFO
module H = Hashtbl
module L = List
module Q = Queue


module Smtmini : Sign.Smt = 
struct

(*--------------------------------------------------------*)
(*                    Variables globales                  *)
(*--------------------------------------------------------*)

let compt = ref 1

let fresh_lit () = 
	let res = M.Lit.make !compt
	in begin 
		incr compt;
		res;
	end

let stolit = H.create 42

let instance = ref (M.create ())

let assump = Q.create ()
let negassump = Q.create ()

let important = ref true

(*--------------------------------------------------------*)
(*                 Fonctions  principales                 *)
(*--------------------------------------------------------*)

let printbug s = 
	print_string s;
	flush_all ()

type ans = 
	| UNSAT
	| SAT of (string*bool) list

let init () = 
begin
	instance := M.create ();
	compt := 1;
	important := true;
	H.clear stolit;
	Q.clear assump;
	Q.clear negassump;
end


let close () = ()

let dec_const s =
	let res = fresh_lit ()
	in begin
		H.add stolit s res;
	end

let tseityn f = 
	let res = ref [] in
	let rec aux = function
	| BFO.Atom p -> H.find stolit p
	| BFO.Not f0 -> 
		let x = fresh_lit ()
		and vf = aux f0
		in begin
			res := [x;vf]::[M.Lit.neg x;M.Lit.neg vf]::(!res);
			x;
		end
	| BFO.Conj (f1,f2) ->
		let x = fresh_lit ()
		and vf1 = aux f1
		and vf2 = aux f2 
		in begin
			res := [M.Lit.neg x; vf1]::[M.Lit.neg x; vf2]
					::[M.Lit.neg vf1; M.Lit.neg vf2; x]
					::(!res);
			x;
		end
	| BFO.Dij (f1,f2) -> 
		let x = fresh_lit () 
		and vf1 = aux f1
		and vf2 = aux f2 
		in begin
			res := [x;M.Lit.neg vf1]::[x; M.Lit.neg vf2]
					::[vf1; vf2;M.Lit.neg x]
					::(!res);
			x;
		end
	in 
	let xf = aux f 
	in begin
		res := [xf]::(!res);
		!res;
	end
		
	

let dec_assert f = 
	try
	let cls = tseityn f in
		List.iter (fun cl -> M.add_clause_l !instance cl) cls
	with M.Unsat -> important := false

let dec_assert_soft f w = 
	let cls = tseityn f
	and g = fresh_lit () 
	in begin
		List.iter 
			(fun cl -> M.add_clause_l !instance ((M.Lit.neg g)::cl))
			cls;
		Q.add g assump;
	end

let get_assump () = 
	let res = ref [] in
	let aux lit = res := lit::(!res)
	and aux_neg lit = res := (M.Lit.neg lit)::(!res)
	in begin	
		Q.iter aux assump;
		Q.iter aux_neg negassump;
		Array.of_list !res;
	end

let get_ans () = 
	if not !important then 
		UNSAT
	else
	let cont = ref true
	and res = ref true 
	in begin
		while !cont do
			cont := false;
			(try M.solve ~assumptions:(get_assump ()) !instance
			with M.Unsat -> 
				if Q.is_empty assump then
					res := false
				else
				begin 
					Q.push (Q.pop assump) negassump;
					cont := true;
				end);
		done;
		if not !res then
			UNSAT
		else
			let model = ref [] in
			let aux s lit =
				match M.value !instance lit with
				| M.V_true -> model := (s,true)::(!model)
				| _ -> model := (s,false)::(!model)
			in begin
				H.iter aux stolit;
				SAT !model;
			end;
	end

			


end
