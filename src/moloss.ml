module L = List
module A = Array
module F = Filename
module U = Unix
module C = Convertisseur
open Lexing

module Dummy  = struct let truc = 0 end


(*          Les différents solveurs            *)
module Sz3 = Solve.Solve(Smtz3.SMTz3)
module Smsat = Solve.Solve(Smtmsat.SMTmsat(Dummy))
module Sminisat = Solve.Solve(Smtminisat.Smtmini)
module D = Direct



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


let truc bidule =
	let lb = Lexing.from_channel bidule in
	Sp_parser.problem Sp_lexer.next_token lb

let solve_vanilla f a =
let module Solv = Solve.Solve in
let argv = A.to_list (Sys.argv) in
	if L.mem "--out" argv then
		fpf "pas encore implem'\n"
	else
		if L.mem "--all" argv then
			let module Z3 = Solv(Smtz3.SMTz3) in
			let module MSAT = Solv(Smtmsat.SMTmsat(Dummy)) in
			let module MiniSAT = Solv(Smtminisat.Smtmini)
			in begin
				fpf "oracle z3\n";
				Z3.solve (C.st "w" f) a |> ignore;
				fpf "oracle mSAT\n";
				MSAT.solve (C.st "w" f) a |> ignore;
				fpf "oracle minisat\n";
				MiniSAT.solve (C.st "w" f) a |> ignore;
			end
		else if L.mem "--z3" argv then
			let module Z3 = Solv(Smtz3.SMTz3)
			in begin
				fpf "oracle z3\n";
				Z3.solve (C.st "w" f) a |> ignore;
			end
		else if L.mem "--mSAT" argv then
			let module MSAT = Solv(Smtmsat.SMTmsat(Dummy) )
			in begin
				fpf "oracle mSAT\n";
				MSAT.solve (C.st "w" f) a |> ignore;
			end
		else
			let module MiniSAT = Solv(Smtminisat.Smtmini)
			in begin
				fpf "oracle minisat\n";
				MiniSAT.solve (C.st "w" f) a |> ignore;
			end


let solve_model f a =
let module Solv = Solve.SolveMod in
let argv = A.to_list (Sys.argv) in
	if L.mem "--out" argv then
		fpf "pas encore implem'\n"
	else
		if L.mem "--all" argv then
			let module Z3 = Solv(Smtz3.SMTz3) in
			let module MSAT = Solv(Smtmsat.SMTmsat(Dummy)) in
			let module MiniSAT = Solv(Smtminisat.Smtmini)
			in begin
				fpf "oracle z3\n";
				Z3.solve (C.st "w" f) a |> ignore;
				fpf "oracle mSAT\n";
				MSAT.solve (C.st "w" f) a |> ignore;
				fpf "oracle minisat\n";
				MiniSAT.solve (C.st "w" f) a |> ignore;
			end
		else if L.mem "--z3" argv then
			let module Z3 = Solv(Smtz3.SMTz3)
			in begin
				fpf "oracle z3\n";
				Z3.solve (C.st "w" f) a |> ignore;
			end
		else if L.mem "--mSAT" argv then
			let module MSAT = Solv(Smtmsat.SMTmsat(Dummy) )
			in begin
				fpf "oracle z3\n";
				MSAT.solve (C.st "w" f) a |> ignore;
			end
		else
			let module MiniSAT = Solv(Smtminisat.Smtmini)
			in begin
				fpf "oracle z3\n";
				MiniSAT.solve (C.st "w" f) a |> ignore;
			end


let _ =
	let argv = A.to_list (Sys.argv)
	and t0 = U.gettimeofday ()
	in begin
	begin
		match argv with


		(*    pour les fichiers .bml, on teste la satisfiabilité    *)
		| _ :: filename :: _ when good_suff filename ->
		begin
		let file = open_in filename in
			let lb = Lexing.from_channel file in
			try
			let a,f = Parser.file Lexer.next_token lb in
				if L.mem "--direct" argv then
				begin
					fpf "oracle direct\n";
					D.solve (C.st "w" f) a |> ignore;
				end
				else if L.mem "--get-model" argv then
					solve_model f a
				else
					solve_vanilla f a
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


		(*   pour les fichiers .dfg, on teste la validité    *)
		| _ ::filename ::_ when F.check_suffix filename ".dfg" ->
		begin
			let file = open_in filename in
			let lb = Lexing.from_channel file
			(* and out =
				if L.mem "--out" argv then
					Some (open_out (new_suff filename))
				else
                                        None *)
			in
			try
			let f,a = Sp_parser.problem Sp_lexer.next_token lb in
				if L.mem "--direct" argv then
					D.solve (C.st "w" f) a |> ignore
				else
				begin
					fpf "Fin du parsing\n";
					flush_all ();
					Sz3.solve (C.st "w" f) a |> ignore;
				end
			with
			| Sp_lexer.Lex_err s ->
			report (lexeme_start_p lb, lexeme_end_p lb) filename;
			fpf "lexical error: %s.\n" s;
			flush_all ();
			exit 1
  			| Sp_parser.Error ->
			report (lexeme_start_p lb, lexeme_end_p lb) filename;
			fpf "syntax error.\n";
			flush_all ();
			exit 1

		end
		| _ ->
		begin
			fpf
			"Donner le nom du fichier avec une extension .bml ou .dfg\n";
			exit 1;
		end;
	end;
		flush_all ();
		if L.mem "--time" argv then
			fpf "Calculs effectués en %f s \n" (U.gettimeofday () -.t0);
	end
