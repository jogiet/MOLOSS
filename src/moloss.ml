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

(** Main function in the solver for vanilla solving
    It's the used function when no option is called (except the solver)*)
let solve_vanilla f =
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
				Printf.printf "c oracle z3\n";
				Z3.solve (Convertisseur.st 0 f) |> ignore;
				Printf.printf "c oracle mSAT\n";
				MSAT.solve (Convertisseur.st 0 f) |> ignore;
				Printf.printf "c oracle minisat\n";
				MiniSAT.solve (Convertisseur.st 0 f) |> ignore;
			end
		else if List.mem "--z3" argv then
    let module Z3 = Solv(Smtz3.SMTz3)(Decision)
			in begin
				Printf.printf "c oracle z3\n";
				Z3.solve (Convertisseur.st 0 f) |> ignore;
			end
		else if List.mem "--mSAT" argv then
    let module MSAT = Solv(Smtmsat.SMTmsat(Dummy))(Decision)
			in begin
				Printf.printf "c oracle mSAT\n";
				MSAT.solve (Convertisseur.st 0 f) |> ignore;
			end
		else
    let module MiniSAT = Solv(Smtminisat.Smtmini)(Decision)
			in begin
				Printf.printf "c oracle minisat\n";
				MiniSAT.solve (Convertisseur.st 0 f) |> ignore;
			end

(** This function solve the formula and ouputs a model *)
let solve_model f =
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
        Printf.printf "c "; Pprinter.print_m f;
				Printf.printf "c oracle z3\n";
				Z3.solve (Convertisseur.st 0 f) |> ignore;
				Printf.printf "c oracle mSAT\n";
				MSAT.solve (Convertisseur.st 0 f) |> ignore;
				Printf.printf "c oracle minisat\n";
				MiniSAT.solve (Convertisseur.st 0 f) |> ignore;
			end
		else if List.mem "--z3" argv then
    let module Z3 = Solv(Smtz3.SMTz3)(Decision)
			in begin
        Printf.printf "c "; Pprinter.print_m f;
				Printf.printf "c oracle z3\n";
				Z3.solve (Convertisseur.st 0 f) |> ignore;
			end
		else if List.mem "--mSAT" argv then
    let module MSAT = Solv(Smtmsat.SMTmsat(Dummy))(Decision)
			in begin
        Printf.printf "c "; Pprinter.print_m f;
				Printf.printf "c oracle mSAT\n";
				MSAT.solve (Convertisseur.st 0 f) |> ignore;
			end
		else
    let module MiniSAT = Solv(Smtminisat.Smtmini)(Decision)
			in begin
        Printf.printf "c "; Pprinter.print_m f;
				Printf.printf "c oracle minisat\n";
				MiniSAT.solve (Convertisseur.st 0 f) |> ignore;
			end

(** This function solve the formula and ouputs the assertions *)
let solve_assert f =
  let module Solv = Solve.SolveAssert in
  let argv = Array.to_list (Sys.argv) in
  if List.mem "--out" argv then
    Printf.printf "pas encore implem'\n"
  else
  if List.mem "--all" argv then
    let module Z3 = Solv(Smtz3.SMTz3)(Decision) in
    let module MSAT = Solv(Smtmsat.SMTmsat(Dummy))(Decision) in
    let module MiniSAT = Solv(Smtminisat.Smtmini)(Decision)
    in begin
      Printf.printf "c "; Pprinter.print_m f;
      Printf.printf "c oracle z3\n";
      Z3.solve (Convertisseur.st 0 f) |> ignore;
      Printf.printf "c oracle mSAT\n";
      MSAT.solve (Convertisseur.st 0 f) |> ignore;
      Printf.printf "c oracle minisat\n";
      MiniSAT.solve (Convertisseur.st 0 f) |> ignore;
    end
  else if List.mem "--z3" argv then
    let module Z3 = Solv(Smtz3.SMTz3)(Decision)
    in begin
      Printf.printf "c "; Pprinter.print_m f;
      Printf.printf "c oracle z3\n";
      Z3.solve (Convertisseur.st 0 f) |> ignore;
    end
  else if List.mem "--mSAT" argv then
    let module MSAT = Solv(Smtmsat.SMTmsat(Dummy))(Decision)
    in begin
      Printf.printf "c "; Pprinter.print_m f;
      Printf.printf "c oracle mSAT\n";
      MSAT.solve (Convertisseur.st 0 f) |> ignore;
    end
  else
    let module MiniSAT = Solv(Smtminisat.Smtmini)(Decision)
    in begin
      Printf.printf "c "; Pprinter.print_m f;
      Printf.printf "c oracle minisat\n";
      MiniSAT.solve (Convertisseur.st 0 f) |> ignore;
    end


let _ =
	let argv = Array.to_list (Sys.argv)
	and t0 = Unix.gettimeofday ()
	in begin
	begin
		match argv with
  	| _ :: filename :: _ when Filename.check_suffix filename ".InToHyLo" ->
    begin
      let file = open_in filename in
      let lb = Lexing.from_channel file in
      try
        let f = InToHyLoParser.file InToHyLoLexer.next_token lb in
        if List.mem "--direct" argv then
          begin
            Printf.printf "c oracle direct\n";
            (Direct.solve (Convertisseur.st 0 f) argv) |> ignore;
          end
        else if List.mem "--get-assert" argv then
          solve_assert f
        else if List.mem "--get-model" argv then
          solve_model f
        else
          solve_vanilla f
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
		| _ ->
		begin
			Printf.printf
			"Give a filename with extension .InToHyLo\n";
			exit 1;
		end;
	end;
		flush_all ();
		if List.mem "--time" argv then
    Printf.printf "Calculs done in %f s \n" (Unix.gettimeofday () -.t0);
	end
