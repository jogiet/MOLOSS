(*########################################################*)
(*               Essai d'un solveur basique               *) 
(*########################################################*)
(*
TODO : rassembler les morceaux de code dans un seul module/dossier : +
correcte
*)

(*--------------------------------------------------------*)
(*              Types & modules manipulés                 *)
(*--------------------------------------------------------*)

open Ast_fo
open Lexing
open Format

module P = Ast_proof

module PP = Pprinter
module U = Unix
module FOMap = Map.Make(FO)
module FOSet = Set.Make(FO)
module Smap = Map.Make(String)
module SSet = Set.Make(String)
module DS = struct
	type t= string*string
	let compare = compare
end
module DSSet = Set.Make(DS)
open Decide


type ident = string



module BFOSet = Set.Make(BFO)

type ans = SAT | UNSAT


let spf = Format.sprintf
and fpf = Format.printf


let aux_out s = function
| None -> ()
| Some oc -> output_string oc s

(*--------------------------------------------------------*)
(*       Fonctions de conversion FO <-> SMT-LIB           *)
(*--------------------------------------------------------*)


let rec bfo_to_smtlib = function
| BFO.Atom i -> i
| BFO.Not f -> spf "(not %s)" (bfo_to_smtlib f)
| BFO.Conj (f1,f2) ->
	spf "(and %s %s)" (bfo_to_smtlib f1) (bfo_to_smtlib f2)
| BFO.Dij (f1,f2) ->
	spf "(or %s %s)" (bfo_to_smtlib f1) (bfo_to_smtlib f2)

let dec_const oc v out = 
	let s = spf "(declare-const %s Bool) \n" v 
	in begin
		output_string oc s;
		aux_out s out;
		flush_all ();
	end


let dec_assert oc bf out = 
	let f_smt = bfo_to_smtlib bf in
	let s = spf "(assert %s) \n" f_smt 
	in begin
		output_string oc s;
		aux_out s out;
		flush_all ();
	end
	

(*--------------------------------------------------------*)
(*         Fonctions pour le parsing du modèle            *)
(*--------------------------------------------------------*)


let report (b,e) f =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  fpf "File \"%s\", line %d, characters %d-%d:\n" f l fc lc
 


let get_model oc ic out = 
begin
	flush_all ();
	output_string oc "(get-model) \n";
	aux_out  "(get-model) \n" out ;
	flush_all ();
	let res = ref ""
	and cont = ref true 
	in begin
		while !cont do 
			let ligne = input_line ic 
			in begin
			res := spf "%s \n%s" !res ligne;
			if ligne = ")" then
				cont := false;
			end;
		done;
		aux_out (!res^"\n") out;
	let lb = Lexing.from_string !res in
	try 
		Z3_parser.answer Z3_lexer.next_token lb
	with

	| Z3_lexer.Lex_err s ->
	report (lexeme_start_p lb, lexeme_end_p lb) "modèle";
	fpf "lexical error: %s.\n" s;
	flush_all ();
	exit 1
    | Z3_parser.Error ->
	report (lexeme_start_p lb, lexeme_end_p lb) "modèle";
	fpf "syntax error.\n";
	flush_all ();
	exit 1
	end;
end

(*--------------------------------------------------------*)
(*        Fonctions pour le parsing de la preuve          *)
(*--------------------------------------------------------*)





let get_proof oc ic out = 
begin
	flush_all ();
	output_string oc "(get-proof) \n";
	aux_out  "(get-proof) \n" out ;
	flush_all ();
	let res = ref ""
	and cont = ref true 
	in begin
		while !cont do 
			let ligne = input_line ic 
			in begin
			res := spf "%s \n%s" !res ligne;
			if ligne = "" then
				cont := false;
			end;
		done;
		aux_out (!res^"\n") out;
	let lb = Lexing.from_string !res in
	try 
		let file = Pparser.s0 Plexer.next_token lb
		in P.traite file
	with

	| Plexer.Lex_err s ->
	report (lexeme_start_p lb, lexeme_end_p lb) "proof";
	fpf "lexical error: %s.\n" s;
	flush_all ();
	exit 1
    | Pparser.Error ->
	report (lexeme_start_p lb, lexeme_end_p lb) "proof";
	fpf "syntax error.\n";
	flush_all ();
	exit 1
	end;
end

(*--------------------------------------------------------*)
(*            Fonctions pour le réponse                   *)
(*--------------------------------------------------------*)
let get_ans oc ic out = 
begin
	output_string oc "(check-sat) \n";
	aux_out "(check-sat) \n" out;
	flush_all ();
	let ans = input_line ic 
	in begin
		aux_out (ans^"\n") out;
		if ans = "unsat" then
			UNSAT
		else
			SAT;
		end;
end
(*--------------------------------------------------------*)
(*              Fonctions pour les axiomes                *)
(*--------------------------------------------------------*)

let axiom_to_dec_proc axiom = 
	let rec aux = function 
	| [] -> []
	| "-M"::q | "-boxeM"::q -> reflexiv::(aux q)
	| "-4"::q -> transitivity::(aux q)
	| "-B"::q -> symmetric::(aux q)
	| "-5"::q -> euclidean::(aux q)
	| "-CD"::q -> functionnal::(aux q)
	| t::q ->
	begin
		fpf "Axiome inconne : %s \n" t;
		exit 1;
	end
	in let res = aux axiom in
	if L.mem "-M" axiom || L.mem  "-CD" axiom  || L.mem "-boxeM" axiom then
		forall::res
	else
		forall::exist::res

let get_init_flag axioms = 
	if L.mem  "-M" axioms then
		[Reflexiv]
	else 
		[]

(*--------------------------------------------------------*)
(*                Fonction de résolution                  *)
(*--------------------------------------------------------*)

let new_config () = 
	let config = 
	{w = H.create 10;
	 env = H.create 10;
	 s = ();
	 exists = H.create 10;
	 forall = H.create 10;
	 sym = H.create 10;
	 trans = H.create 10;

	 euc = H.create 10;
	 fonc = H.create 10}
	 in begin 
	 	H.add config.w "w" ();
		config;
	end

let rec init (config : config) init_flag = function 
(* 
renvoie la liste des fonctions encadrées, et les nouvelles variables 
et enrichit la config au fur et à mesure ...
*)
| [] -> 
	if List.mem Reflexiv init_flag then
		let fb,new_var = abs config.env (FO.Relation ("w","w"))
		in [fb],new_var
	else
		[],[]
| f::q -> 
	let f_box,new_var = abs config.env f in
	let f_rest,new_var_rest = init config init_flag q in
		f_box::f_rest, new_var@new_var_rest


let print_soluce config m = 
let aux (k,b) = 
	let f = H.find config.env k in
	match b with
	| true -> PP.print_fo f
	| false -> PP.print_fo (FO.Not f)
in begin
	fpf "liste des mondes : \n";
	H.iter (fun k _ -> fpf "%s \n" k) config.w;
	flush_all ();
	fpf "\nPropriétés à vérifier :\n";	
	List.iter aux m;
	flush_all ();
end


let solve f a out = 
	let config = new_config () in
	let init_flag = get_init_flag a
	and dec_proc = axiom_to_dec_proc a in	
	let fo_box, new_var = init config init_flag [f] in
	let ic,oc = U.open_process "./z3 -in"
	and cont = ref true 
	and s = "(set-option :produce-proofs true)\n"
	in begin
		output_string oc s;
		aux_out s out;
		L.iter (fun v -> dec_const oc v out) new_var; 
		L.iter (fun fb -> dec_assert oc fb out) fo_box	;
		while !cont do
			match get_ans oc ic out with
			| UNSAT ->
			let p = get_proof oc ic out
			in begin
				fpf "La formule est insatisfiable \n";
				PP.print_proof config.env p;
				flush_all ();
				cont := false; 
			end
			| SAT -> let m = get_model oc ic out in
				try begin
					L.iter (fun d_proc -> d_proc config m) dec_proc;
					fpf "La formule est satisfiable \n";
					print_soluce config m;
					flush_all ();
					cont := false; 
				end
				with
				| Found (new_var,new_bf) ->
				begin
					List.iter (fun v -> dec_const oc v out) new_var;
					dec_assert oc new_bf out;
				end;
		done;
		fpf "Terminé !!! \n";
		flush_all ();
	end
					
			

























