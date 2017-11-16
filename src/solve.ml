(*########################################################*)
(*                 Foncteur de résolution                 *)
(*########################################################*)


open Format
open Ast_fo
open Decide

module L = List

module PP = Pprinter

let fpf = printf

(*--------------------------------------------------------*)
(*                    Fonction Core                       *)
(*--------------------------------------------------------*)

module Solve (SMT : Sign.Smt)(D : Sign.Decide) : Sign.Solveur=
struct



let solve f =
	let config = D.new_config () in
	(* let init_flag = get_init_flag a in *)
	(* and dec_proc = axiom_to_dec_proc a in	 *)
	let fo_box, new_var = D.init config [f]
	and cont = ref true
	and res = ref true
	in
	begin
		SMT.init ();
		L.iter (fun v -> SMT.dec_const v ) new_var;
		L.iter (fun fb -> SMT.dec_assert fb ) fo_box	;
		while !cont do
			match SMT.get_ans () with
			| SMT.UNSAT ->
			let p = () (*      get_proof oc ic out*)
			in begin
				fpf "c \027[31mUNSAT\027[0m\n";
				p |> ignore;
				flush_all ();
				cont := false;
				res := false;
			end
			| SMT.SAT  m ->
				try begin
					D.decide config m;
					fpf "c \027[92mSAT\027[0m\n";
					(*
					print_soluce config m;
					*)
					flush_all ();
					cont := false;
				end
				with
				| D.Found (new_var,new_bf) ->
				begin
					List.iter (fun v -> SMT.dec_const v) new_var;
					SMT.dec_assert new_bf ;
				end;
				| D.SoftFound (new_var1,new_bf,new_var2,bf_soft,wght) ->
				begin
					List.iter (fun v -> SMT.dec_const v ) new_var1;
					List.iter (fun v -> SMT.dec_const v ) new_var2;
					SMT.dec_assert new_bf;
					SMT.dec_assert_soft bf_soft wght;
				end
		done;
		SMT.close ();
		!res;
	end
end


module SolveMod (SMT : Sign.Smt)(D : Sign.Decide) : Sign.Solveur=
struct
(** For option --get-model    *)


let solve f  =
let config = D.new_config () in
let fo_box, new_var = D.init config [f]
and cont = ref true
and res = ref true
in
begin
	SMT.init ();
	L.iter (fun v -> SMT.dec_const v ) new_var;
	L.iter (fun fb -> SMT.dec_assert fb ) fo_box	;
	begin
		SMT.init ();
		L.iter (fun v -> SMT.dec_const v ) new_var;
		L.iter (fun fb -> SMT.dec_assert fb ) fo_box	;
		while !cont do
			match SMT.get_ans () with
			| SMT.UNSAT ->
			let p = () (*      get_proof oc ic out*)
			in begin
				fpf "c \027[31mUNSAT\027[0m\n";
				p |> ignore;
				flush_all ();
				cont := false;
				res := false;
			end
			| SMT.SAT  m ->
				try begin
					D.decide config m;
          print_string "s SATISFIABLE\n";
          flush_all ();
					D.print_model config m;
					(* flush_all (); *)
					cont := false;
				end
				with
				| D.Found (new_var,new_bf) ->
				begin
					List.iter (fun v -> SMT.dec_const v) new_var;
					SMT.dec_assert new_bf ;
				end;
				| D.SoftFound (new_var1,new_bf,new_var2,bf_soft,wght) ->
				begin
					List.iter (fun v -> SMT.dec_const v ) new_var1;
					List.iter (fun v -> SMT.dec_const v ) new_var2;
					SMT.dec_assert new_bf;
					SMT.dec_assert_soft bf_soft wght;
				end
		done;
		SMT.close ();
		!res;
	end
end
end
