module SMTz3 : Sign.Smt = 
struct

module BFO = Ast_fo.BFO
module P = Ast_proof
module U = Unix
open Lexing


let fpf = Format.printf
let spf = Format.sprintf

let ic = ref stdin
and oc = ref stdout



let init () = 
	let ic_aux,oc_aux = U.open_process "./z3 -in"
	in begin
		ic := ic_aux;
		oc := oc_aux;
	end

let close () = 
	U.close_process (!ic,!oc) |> ignore

(*--------------------------------------------------------*)
(*       Fonctions de conversion FO <-> SMT-LIB           *)
(*--------------------------------------------------------*)

type ans = 
	| UNSAT
	| SAT of (string*bool) list




let rec bfo_to_smtlib = function
| BFO.Atom i -> i
| BFO.Not f -> spf "(not %s)" (bfo_to_smtlib f)
| BFO.Conj (f1,f2) ->
	spf "(and %s %s)" (bfo_to_smtlib f1) (bfo_to_smtlib f2)
| BFO.Dij (f1,f2) ->
	spf "(or %s %s)" (bfo_to_smtlib f1) (bfo_to_smtlib f2)

let dec_const v = 
	let s = spf "(declare-const %s Bool) \n" v 
	in begin
		output_string !oc s;
		flush_all ();
	end


let dec_assert bf = 
	let f_smt = bfo_to_smtlib bf in
	let s = spf "(assert %s) \n" f_smt 
	in begin
		output_string !oc s;
		flush_all ();
	end
	
let dec_assert_soft bf weight = 
	let f_smt = bfo_to_smtlib bf in
	let s = spf "(assert-soft %s :weight %d)" f_smt weight 
	in begin
		output_string !oc s;
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
 


let get_model () = 
begin
	flush_all ();
	output_string !oc "(get-model) \n";
	flush_all ();
	let res = ref ""
	and cont = ref true 
	in begin
		while !cont do 
			let ligne = input_line !ic 
			in begin
			if ligne = "(objectives" then
			begin
				input_line !ic |> ignore; (*  (1) *)
				input_line !ic |> ignore; (* )    *)
			end
			else
				res := spf "%s \n%s" !res ligne;
			if ligne = ")" then
				cont := false;
			end;
		done;
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
(*            Fonctions pour la réponse                   *)
(*--------------------------------------------------------*)
let get_ans () = 
begin
	output_string !oc "(check-sat) \n";
	flush_all ();
	let ans = input_line !ic 
	in begin
		if ans = "unsat" then
			UNSAT
		else
			SAT (get_model ())
		end;
end

end
