(*########################################################*)
(*        Solveur qui envoie tout le boulot à Z3          *)
(*########################################################*)
module L = List
module H = Hashtbl
module A = Array
module F = Filename
module U = Unix
module C = Convertisseur
module M = Ast_modal
module FO = Ast_fo.FO
open Lexing


(*--------------------------------------------------------*)
(*              Quelques fonctions annexes                *)
(*--------------------------------------------------------*)

let fpf = Printf.printf
let spf = Printf.sprintf

let p_out s = function
	| None -> ()
	| Some oc -> output_string oc s

let report (b,e) file =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
fpf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let good_suff s = 
	F.check_suffix s ".bml"
let new_suff s = 
	(F.chop_suffix s ".bml")^".out"

let init oc out =
	let header = 
	"(set-option :produce-proofs true) \n\
	(declare-sort W 0) \n\
	(declare-fun r (W W) Bool) \n\
	(declare-const w W) \n"
	in begin
		output_string oc header;
		p_out header out;
	end


(*--------------------------------------------------------*)
(*                Gestion des axiomes                     *)
(*--------------------------------------------------------*)

let assoc = 
[("-M","(assert (forall ((w0 W)) (r w0 w0)))");
 ("-4","(assert (forall ((w0 W) (w1 W) (w2 W)) \
 	(=> (and (r w0 w1) (r w1 w2)) (r w0 w2))))\n");
 ("-B","(assert (forall ((w1 W)(w2 W)) \
 	(=> (r w1 w2) (r w2 w1))))\n");
 ("-5","(assert (forall ((w0 W) (w1 W) (w2 W)) \
 	(=> (and (r w0 w1) (r w0 w2)) \
		(and (r w1 w2) (r w2 w1)))))\n");
 ("-CD","(assert (forall ((w0 W) (w1 W) (w2 W)) \
 	(=> (and (r w0 w1) (r w0 w2)) \
		(= w1 w2))))\n")]

let rec p_axiom oc out = function
| [] -> ()
| t::q ->
begin
	if L.mem_assoc t assoc then
		let ax = L.assoc t assoc 
		in begin
			output_string oc ax;
			p_out ax out;
		end
	else
	begin	
		fpf "Erreur : axiome inconnu : %s \n" t;
		flush_all () ;
		exit 0;
	end;
	p_axiom oc out q;
end
	

(*--------------------------------------------------------*)
(*               fonctions d'écriture                     *)
(*--------------------------------------------------------*)

(* ====>      Pour les propriétés (ensemble P)      <==== *)


let extract_p f = 
(* extrait l'ensemble P des propoitions *)
	let rec aux env = function
	| FO.Atom (p,_) ->
		if H.mem env p then ()
				   	   else H.add env p ()
	| FO.Not f -> aux env f
	| FO.Conj (f1,f2) | FO.Dij (f1,f2) -> 
	begin
		aux env f1;
		aux env f2;
	end
	| FO.Relation _ -> ()
	| FO.Exists (i,f) | FO.Forall (i,f) -> 
		aux env f
	and env = H.create 42
	and res = ref []
	in begin	
		aux env f;
		H.iter (fun p h -> res := p::(!res))  env;
		!res;
	end

let p_prop oc out f = 
	(* déclare les propositions comme des fonctions prenant en argument
	un monde *)
	let aux p = 
		let s = spf "(declare-fun %s (W) Bool)\n" p
		in begin
			output_string oc s;
			p_out s out;
		end
	in 
	L.iter aux (extract_p f)
	
(* ====>            Gestion des formules            <==== *)

let assert_of_for f =
	let rec aux_fo = function
	(* Les parnthèses sont géreés par l'appelant *)
	| FO.Atom (p,x) -> spf "%s %s" p x
	| FO.Not f -> spf "not (%s)" (aux_fo f)
	| FO.Conj (f1,f2) -> spf "and (%s) (%s)" (aux_fo f1) (aux_fo f2)
	| FO.Dij (f1,f2) -> spf "or (%s) (%s)" (aux_fo f1) (aux_fo f2)
	| FO.Relation (x,y) -> spf "r %s %s" x y
	| FO.Exists (i,f) -> spf "exists ((%s W)) (%s)" i (aux_fo f)
	| FO.Forall (i,f) -> spf "forall ((%s W)) (%s)" i (aux_fo f)
	in
	spf "(assert (%s))\n" (aux_fo f)

let p_for oc out f = 
	let s = assert_of_for f
	in begin
		output_string oc s;
		p_out s out;
	end


(*--------------------------------------------------------*)
(*                Fonctions de dialogue                   *)
(*--------------------------------------------------------*)

type ret = |SAT |UNSAT

let check_sat ic oc out = 
	let s = "(check-sat)\n"
	in begin
		output_string oc s;
		p_out s out;
		flush_all ();
		if (input_line ic = "unsat") then
		begin
			p_out "unsat\n" out; 
			UNSAT;
		end
		else
		begin
			p_out "sat\n" out; 
			SAT;
		end;
	end

let get_model ic oc out = 
	let s = "(get-model)\n"
	and res = ref ""
	and cont = ref true
	in begin
		output_string oc s;
		p_out s out;
		flush_all ();
		while !cont do 
			let l = input_line ic 
			in begin
				res := spf "%s\n%s" !res l;
				if l = ")" then
					cont := false;
			end;
		done;
		res := !res^"\n";
		p_out !res out;
		!res;
	end

(*--------------------------------------------------------*)
(*                 Coeur du programme                     *)
(*--------------------------------------------------------*)


let _ = 
	let argv = A.to_list (Sys.argv) 
	and t0 = U.gettimeofday ()
	and ic,oc = U.open_process "./z3 -in"
	in begin
	begin
		match argv with
		| _ :: filename :: _ when good_suff filename ->
		begin			
		let file = open_in filename in
			let lb = Lexing.from_channel file 
			and out = 
				if L.mem "--out" argv then 
					Some (open_out (new_suff filename))
				else
					None
			in
			try			
			let a,f = Parser.file Lexer.next_token lb in
			let fo = C.st "w" f
			in begin
				init oc out;
				p_axiom oc out a;
				p_prop oc out fo;
				p_for oc out fo;
				match check_sat ic oc out with
				| UNSAT -> 
					let s = "la formule est insatisfiable \n"
					in begin
						fpf "%s" s;
					end
				| SAT -> 
					let s = "la formule est satisfiable \n"
					and m = get_model ic oc out 
					in begin
						fpf "%s" s;
						fpf "%s" m;
					end
			end
			with
			| Lexer.Lex_err s ->
			report (lexeme_start_p lb, lexeme_end_p lb) filename;
			fpf "lexical error: %s.\n" s;
			flush_all ();
			exit 1
  			| Parser.Error ->
			report (lexeme_start_p lb, lexeme_end_p lb) filename;
			fpf "syntax error.\n";
			flush_all ();
			exit 1
		end					
		| _ ->
		begin
			fpf "Donner le nom du fichier avec une extension .bml\n";
			exit 1;
		end;
	end;
		if L.mem "--time" argv then
			fpf "Claculs effectués en %f s \n" (U.gettimeofday () -.t0);
	end





































