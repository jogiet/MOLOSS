(** Solveur's main module  *)

open Lexing

module Dummy  = struct let truc = 0 end


module DecisionArg : Sign.DecideArg =
struct

  let argument = 
    let _ = Cmdline.init () in
      Cmdline.(!axs)
end

module Decision = Decide.GetDecide(DecisionArg)
module Simplify = Simplify.GetSimplify(DecisionArg)

(*          Les différents solveurs            *)
module Sz3 = Solve.Solve(Smtz3.SMTz3)(Decision)(Simplify)
module Smsat = Solve.Solve(Smtmsat.SMTmsat(Dummy))(Decision)(Simplify)
module Sminisat = Solve.Solve(Smtminisat.Smtmini)(Decision)(Simplify)
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
		if Cmdline.(!optAll) then
    let module Z3 = Solv(Smtz3.SMTz3)(Decision)(Simplify) in
    let module MSAT = Solv(Smtmsat.SMTmsat(Dummy))(Decision)(Simplify) in
    let module MiniSAT = Solv(Smtminisat.Smtmini)(Decision)(Simplify)
			in begin
				Printf.printf "c oracle z3\n";
				Z3.solve f |> ignore;
				Printf.printf "c oracle mSAT\n";
				MSAT.solve f |> ignore;
				Printf.printf "c oracle minisat\n";
				MiniSAT.solve f |> ignore;
			end
		else if Cmdline.(!optZ3) then
    let module Z3 = Solv(Smtz3.SMTz3)(Decision)(Simplify)
			in begin
				Printf.printf "c oracle z3\n";
				Z3.solve f |> ignore;
			end
		else if Cmdline.(!optmSAT) then
    let module MSAT = Solv(Smtmsat.SMTmsat(Dummy))(Decision)(Simplify)
			in begin
				Printf.printf "c oracle mSAT\n";
				MSAT.solve f |> ignore;
			end
		else
    let module MiniSAT = Solv(Smtminisat.Smtmini)(Decision)(Simplify)
			in begin
				Printf.printf "c oracle minisat\n";
				MiniSAT.solve f |> ignore;
			end

(** This function solve the formula and ouputs a model *)
let solve_model f =
let module Solv = Solve.SolveMod in
let argv = Array.to_list (Sys.argv) in
	if List.mem "--out" argv then
		Printf.printf "pas encore implem'\n"
	else
		if Cmdline.(!optAll) then
    let module Z3 = Solv(Smtz3.SMTz3)(Decision)(Simplify) in
    let module MSAT = Solv(Smtmsat.SMTmsat(Dummy))(Decision)(Simplify) in
    let module MiniSAT = Solv(Smtminisat.Smtmini)(Decision)(Simplify)
      in begin
        Printf.printf "c "; Pprinter.print_m f;
				Printf.printf "c oracle z3\n";
				Z3.solve f |> ignore;
				Printf.printf "c oracle mSAT\n";
				MSAT.solve f |> ignore;
				Printf.printf "c oracle minisat\n";
				MiniSAT.solve f |> ignore;
			end
		else if Cmdline.(!optZ3) then
    let module Z3 = Solv(Smtz3.SMTz3)(Decision)(Simplify)
			in begin
        Printf.printf "c "; Pprinter.print_m f;
				Printf.printf "c oracle z3\n";
				Z3.solve f |> ignore;
			end
		else if Cmdline.(!optmSAT) then
    let module MSAT = Solv(Smtmsat.SMTmsat(Dummy))(Decision)(Simplify)
			in begin
        Printf.printf "c "; Pprinter.print_m f;
				Printf.printf "c oracle mSAT\n";
				MSAT.solve f |> ignore;
			end
		else
    let module MiniSAT = Solv(Smtminisat.Smtmini)(Decision)(Simplify)
			in begin
        Printf.printf "c "; Pprinter.print_m f;
				Printf.printf "c oracle minisat\n";
				MiniSAT.solve f |> ignore;
			end

(** This function solve the formula and ouputs the assertions *)
let solve_assert f =
  let module Solv = Solve.SolveAssert in
  let argv = Array.to_list (Sys.argv) in
  if List.mem "--out" argv then
    Printf.printf "pas encore implem'\n"
  else
	if Cmdline.(!optAll) then
    let module Z3 = Solv(Smtz3.SMTz3)(Decision)(Simplify) in
    let module MSAT = Solv(Smtmsat.SMTmsat(Dummy))(Decision)(Simplify) in
    let module MiniSAT = Solv(Smtminisat.Smtmini)(Decision)(Simplify)
    in begin
      Printf.printf "c "; Pprinter.print_m f;
      Printf.printf "c oracle z3\n";
      Z3.solve f |> ignore;
      Printf.printf "c oracle mSAT\n";
      MSAT.solve f |> ignore;
      Printf.printf "c oracle minisat\n";
      MiniSAT.solve f |> ignore;
    end
	else if Cmdline.(!optZ3) then
    let module Z3 = Solv(Smtz3.SMTz3)(Decision)(Simplify)
    in begin
      Printf.printf "c "; Pprinter.print_m f;
      Printf.printf "c oracle z3\n";
      Z3.solve f |> ignore;
    end
	else if Cmdline.(!optmSAT) then
    let module MSAT = Solv(Smtmsat.SMTmsat(Dummy))(Decision)(Simplify)
    in begin
      Printf.printf "c "; Pprinter.print_m f;
      Printf.printf "c oracle mSAT\n";
      MSAT.solve f |> ignore;
    end
  else
    let module MiniSAT = Solv(Smtminisat.Smtmini)(Decision)(Simplify)
    in begin
      Printf.printf "c "; Pprinter.print_m f;
      Printf.printf "c oracle minisat\n";
      MiniSAT.solve f |> ignore;
    end


let _ =
  let _ = Cmdline.init () in
  let t0 = Unix.gettimeofday ()
	in begin
	begin
		match Cmdline.(!file) with
  	| Some filename ->
    begin
      let file = open_in filename in
      let lb = Lexing.from_channel file in
      try
        let f = InToHyLoParser.file InToHyLoLexer.next_token lb in
        if Cmdline.(!optDirect) then
          begin
            Printf.printf "c oracle direct\n";
            (Direct.solve (Convertisseur.st 0 f) Cmdline.(!axs)) |> ignore;
          end
        else if Cmdline.(!optGetSimplify) then
          let fs = Simplify.simplify f in
          let l = Ast_modal.formLength f
          and d = Ast_modal.modDegree f
          and ls = Ast_modal.formLength fs
          and ds = Ast_modal.modDegree fs in
          let cls = if ls < l
            then "\027[32m"
            else "\027[31m"
          and cds = if ds < d
						then "\027[32m"
						else "\027[31m"
          in
            Printf.printf "%d,%d,%s%d\027[0m,%s%d\027[0m\n" l d cls ls cds ds
        else if Cmdline.(!optGetAssert) then
          solve_assert f
        else if Cmdline.(!optGetModel) then
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
      Cmdline.print_usage ();
			exit 1;
		end;
	end;
		flush_all ();
		if Cmdline.(!optTime) then
    Printf.printf "c Done in %f s \n" (Unix.gettimeofday () -.t0);
	end
