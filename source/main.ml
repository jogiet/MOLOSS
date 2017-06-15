module L = List
module A = Array
module F = Filename
module U = Unix
module C = Convertisseur
module So = Solve 
open Lexing

let fpf = Printf.printf

let report (b,e) file =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
fpf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc


let good_suff s = 
	F.check_suffix s ".bml"
let new_suff s = 
	(F.chop_suffix s ".bml")^".out"



let _ = 
	let argv = A.to_list (Sys.argv) 
	and t0 = U.gettimeofday ()
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
			let f = Parser.file Lexer.next_token lb in
				So.solve (C.st "w" f) out
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
			 

			































