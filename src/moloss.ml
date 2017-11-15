(** Solveur's main module  *)

open Lexing

module Dummy  = struct let truc = 0 end

module DecisionArg : Sign.DecideArg =
struct
  let argument = match (Array.to_list (Sys.argv)) with
    | _ :: _ :: q ->
      q
    | _  ->
      Printf.printf "Give a formula to solve\n";
      exit 0
end

module Decision = Decide.GetDecide(DecisionArg)

(*          Les différents solveurs            *)
module Sz3 = Solve.Solve(Smtz3.SMTz3)(Decision)
module Smsat = Solve.Solve(Smtmsat.SMTmsat(Dummy))(Decision)
module Sminisat = Solve.Solve(Smtminisat.Smtmini)(Decision)
module D = Direct


(** A basic function to report parsing/lexing bug *)
let report (b,e) file =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
Printf.printf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc


(** Returns true if the string given in argument as suffix ".bml" *)
let good_suffixe s =
  Filename.check_suffix s ".bml"

(** Returns the filename with the .out suffix insteadof .bml suffix *)
let new_suffixe s =
	(Filename.chop_suffix s ".bml")^".out"

(** Main function in the solver for vanilla solving
    It's the used function when no option is called (except the solver)*)
let solve_vanilla f a =
let module Solv = Solve.Solve in
let argv = Array.to_list (Sys.argv) in
	if List.mem "--out" argv then
		Printf.printf "pas encore implem'\n"
	else
		if List.mem "--all" argv then
    let module Z3 = Solv(Smtz3.SMTz3)(Decision) in
    let module MSAT = Solv(Smtmsat.SMTmsat(Dummy))(Decision) in
    let module MiniSAT = Solv(Smtminisat.Smtmini)(Decision)
			in begin
				Printf.printf "oracle z3\n";
				Z3.solve (Convertisseur.st 0 f) a |> ignore;
				Printf.printf "oracle mSAT\n";
				MSAT.solve (Convertisseur.st 0 f) a |> ignore;
				Printf.printf "oracle minisat\n";
				MiniSAT.solve (Convertisseur.st 0 f) a |> ignore;
			end
		else if List.mem "--z3" argv then
    let module Z3 = Solv(Smtz3.SMTz3)(Decision)
			in begin
				Printf.printf "oracle z3\n";
				Z3.solve (Convertisseur.st 0 f) a |> ignore;
			end
		else if List.mem "--mSAT" argv then
    let module MSAT = Solv(Smtmsat.SMTmsat(Dummy))(Decision)
			in begin
				Printf.printf "oracle mSAT\n";
				MSAT.solve (Convertisseur.st 0 f) a |> ignore;
			end
		else
    let module MiniSAT = Solv(Smtminisat.Smtmini)(Decision)
			in begin
				Printf.printf "oracle minisat\n";
				MiniSAT.solve (Convertisseur.st 0 f) a |> ignore;
			end

(** This function solve the formula and ouputs a model *)
let solve_model f a =
let module Solv = Solve.SolveMod in
let argv = Array.to_list (Sys.argv) in
	if List.mem "--out" argv then
		Printf.printf "pas encore implem'\n"
	else
		if List.mem "--all" argv then
      let module Z3 = Solv(Smtz3.SMTz3)(Decision) in
      let module MSAT = Solv(Smtmsat.SMTmsat(Dummy))(Decision) in
      let module MiniSAT = Solv(Smtminisat.Smtmini)(Decision)
			in begin
				Printf.printf "oracle z3\n";
				Z3.solve (Convertisseur.st 0 f) a |> ignore;
				Printf.printf "oracle mSAT\n";
				MSAT.solve (Convertisseur.st 0 f) a |> ignore;
				Printf.printf "oracle minisat\n";
				MiniSAT.solve (Convertisseur.st 0 f) a |> ignore;
			end
		else if List.mem "--z3" argv then
    let module Z3 = Solv(Smtz3.SMTz3)(Decision)
			in begin
				Printf.printf "oracle z3\n";
				Z3.solve (Convertisseur.st 0 f) a |> ignore;
			end
		else if List.mem "--mSAT" argv then
    let module MSAT = Solv(Smtmsat.SMTmsat(Dummy))(Decision)
			in begin
				Printf.printf "oracle mSAT\n";
				MSAT.solve (Convertisseur.st 0 f) a |> ignore;
			end
		else
    let module MiniSAT = Solv(Smtminisat.Smtmini)(Decision)
			in begin
				Printf.printf "oracle minisat\n";
				MiniSAT.solve (Convertisseur.st 0 f) a |> ignore;
			end


let _ =
	let argv = Array.to_list (Sys.argv)
	and t0 = Unix.gettimeofday ()
	in begin
	begin
		match argv with


		(*    pour les fichiers .bml, on teste la satisfiabilité    *)
      (*
    | _ :: filename :: _ when Filename.check_suffix filename ".bml" ->

    begin
		let file = open_in filename in
			let lb = Lexing.from_channel file in
			try
			let a,f = Parser.file Lexer.next_token lb in
				if List.mem "--direct" argv then
				begin
					Printf.printf "oracle direct\n";
					Direct.solve (Convertisseur.st 0 f) a |> ignore;
				end
				else if List.mem "--get-model" argv then
					solve_model f a
				else
					solve_vanilla f a
			with
			| Lexer.Lex_err s ->
			report (lexeme_start_p lb, lexeme_end_p lb) filename;
			Printf.printf "lexical error: %s.\n" s;
			flush_all ();
			exit 1
  			| Parser.Error ->
			report (lexeme_start_p lb, lexeme_end_p lb) filename;
			Printf.printf "syntax error.\n";
			flush_all ();
			exit 1
	end
      *)
  	| _ :: filename :: _ when Filename.check_suffix filename ".InToHyLo" ->
    begin
      let file = open_in filename in
      let lb = Lexing.from_channel file in
      try
        let a,f = InToHyLoParser.file InToHyLoLexer.next_token lb in
        if List.mem "--direct" argv then
          begin
            Printf.printf "oracle direct\n";
            Direct.solve (Convertisseur.st 0 f) a |> ignore;
          end
        else if List.mem "--get-model" argv then
          solve_model f a
        else
          solve_vanilla f a
      with
      | InToHyLoLexer.Lex_err s ->
        report (lexeme_start_p lb, lexeme_end_p lb) filename;
        Printf.printf "lexical error: %s.\n" s;
        flush_all ();
        exit 1
      | InToHyLoParser.Error ->
        report (lexeme_start_p lb, lexeme_end_p lb) filename;
        Printf.printf "syntax error.\n";
        flush_all ();
        exit 1
    end

      (*
		(*   pour les fichiers .dfg, on teste la validité    *)
		| _ ::filename ::_ when Filename.check_suffix filename ".dfg" ->
		begin
			let file = open_in filename in
			let lb = Lexing.from_channel file
			(* and out =
				if List.mem "--out" argv then
					Some (open_out (new_suff filename))
				else
                                        None *)
			in
			try
			let f,a = Sp_parser.problem Sp_lexer.next_token lb in
				if List.mem "--direct" argv then
					Direct.solve (Convertisseur.st 0 f) a |> ignore
				else
				begin
					Printf.printf "Fin du parsing\n";
					flush_all ();
					Sz3.solve (Convertisseur.st 0 f) a |> ignore;
				end
			with
			| Sp_lexer.Lex_err s ->
			report (lexeme_start_p lb, lexeme_end_p lb) filename;
			Printf.printf "lexical error: %s.\n" s;
			flush_all ();
			exit 1
  			| Sp_parser.Error ->
			report (lexeme_start_p lb, lexeme_end_p lb) filename;
			Printf.printf "syntax error.\n";
			flush_all ();
			exit 1

end
*)
		| _ ->
		begin
			Printf.printf
			"Donner le nom du fichier avec une extension .InToHyLo\n";
			exit 1;
		end;
	end;
		flush_all ();
		if List.mem "--time" argv then
			Printf.printf "Calculs effectués en %f s \n" (Unix.gettimeofday () -.t0);
	end
