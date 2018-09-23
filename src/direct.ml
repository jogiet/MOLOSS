(*########################################################*)
(*        Solveur qui envoie tout le boulot à Z3          *)
(*########################################################*)
module L = List
module H = Hashtbl
module A = Array
module F = Filename
module U = Unix
module FO = Ast_fo.FO
module PP = Pprinter
open Lexing

(**
   This module solves the formula using Z3 as a SMT-solver
   (with FO-logic)
*)


(*--------------------------------------------------------*)
(*              Quelques fonctions annexes                *)
(*--------------------------------------------------------*)

(**/**) (* We don't want shortcuts in the doc *)
let fpf = Printf.printf
let spf = Printf.sprintf
(**/**)

    (** Outputs the string if the channel is defined *)
let p_out s = function
	| None -> ()
	| Some oc -> output_string oc s

    (** Basic function to report errors in Lexing/Parsing *)
let report (b,e) file =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
fpf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let good_suff s =
	F.check_suffix s ".bml"
let new_suff s =
	(F.chop_suffix s ".bml")^".out"

(** Prints the header :
    - the options for proofs and models production,
    - the FO sort, and the FO predicate for the relation
*)
let init oc out =
	let header =
  "(set-option :produce-proofs true) \n\
	(set-option :produce-models true) \n\
	(declare-sort W 0) \n\
	(declare-fun r (W W) Bool) \n\
	(declare-const w0 W) \n"
	in begin
		output_string oc header;
		p_out header out;
	end


(*--------------------------------------------------------*)
(*               Code pour des UniqueId                   *)
(*--------------------------------------------------------*)

(**/**)
let index = ref 0
(**/**)

(** Returns a fresh int *)
let getUniqueId () =
  let _  = incr index in
  	!index

(*--------------------------------------------------------*)
(*                Gestion des axiomes                     *)
(*--------------------------------------------------------*)

(** An association list between the axioms and their FO translation *)
let assoc =
[(Ast_modal.AxS,";axiome de réfléxivité \n\
	(assert (forall ((w0 W)) (r w0 w0)))");
 (Ast_modal.Ax4,";axiome de trasitivité \n\
 	(assert (forall ((w0 W) (w1 W) (w2 W)) \
			(=> (and (r w0 w1) (r w1 w2)) (r w0 w2))))\n");
 (Ast_modal.AxB,";axiome de symétrie \n\
 	(assert (forall ((w1 W)(w2 W)) \
 			(=> (r w1 w2) (r w2 w1))))\n");
 (Ast_modal.Ax5,":axiome de euclidienne \n\
 	(assert (forall ((w0 W) (w1 W) (w2 W)) \
 			(=> (and (r w0 w1) (r w0 w2)) \
				(and (r w1 w2) (r w2 w1)))))\n");
 (Ast_modal.AxCD,";axiome de fonctionelle \n\
 	(assert (forall ((w0 W) (w1 W) (w2 W)) \
 			(=> (and (r w0 w1) (r w0 w2)) \
				(= w1 w2))))\n")]

(** Give the z3 solver the frame axioms *)
let rec p_axiom oc out = function
| [] -> ()
| t::q ->
begin
	if L.mem_assoc t assoc then
		let ax = L.assoc t assoc
		in begin
			output_string oc ax;
			p_out ax out;
	    p_axiom oc out q;
		end
	else
    assert false
end


(*--------------------------------------------------------*)
(*               fonctions d'écriture                     *)
(*--------------------------------------------------------*)

(* ====>      Pour les propriétés (ensemble P)      <==== *)

   (** extract the set of FO unary predicates *)
let extract_p f =
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
  (** Declare unary predicates to Z3 *)
	let aux p =
		let s = spf "(declare-fun P%d (W) Bool)\n" p
		in begin
			output_string oc s;
			p_out s out;
		end
	in
	L.iter aux (extract_p f)

(* ====>            Gestion des formules            <==== *)

(** Returns the string equivalent to the assertion of the formula in
    SMT-LIB format *)
let assert_of_for f =
  let print_world env x =
    if x >= 0 then
      spf "w%d" x
    else
      spf "w%s" (H.find env x) in
	let rec aux_fo env = function
	(* Les parnthèses sont géreés par l'appelant *)
  | FO.Atom (p,x) ->
    spf "P%d %s" p (print_world env x)
	| FO.Not f -> spf "not (%s)" (aux_fo env f)
	| FO.Conj (f1,f2) -> spf "and (%s) (%s)" (aux_fo env f1) (aux_fo env f2)
	| FO.Dij (f1,f2) -> spf "or (%s) (%s)" (aux_fo env f1) (aux_fo env f2)
 	| FO.Relation (x,y) ->
    spf "r %s %s" (print_world env x) (print_world env y)
 	| FO.Exists (i,f) ->
   	let index = getUniqueId () in
   	let _ = H.add env i (spf "k%d" index) in
   		spf "exists ((wk%d W)) (%s)" index (aux_fo env f)
 	| FO.Forall (i,f) ->
		let index = getUniqueId () in
   	let _ = H.add env i (spf "k%d" index) in
   		spf "forall ((wk%d W)) (%s)" index (aux_fo env f)
	in
 	spf "(assert (%s))\n" (aux_fo (H.create 17) f)


let p_for oc out f =
  (* Asserts the formula to Z3 *)
	let s = assert_of_for f
	in begin
		output_string oc s;
		p_out s out;
	end


(*--------------------------------------------------------*)
(*                Fonctions de dialogue                   *)
(*--------------------------------------------------------*)

    (** A basic type for the results of the Z3-solver *)
type ret =
|SAT
|UNSAT

(** Asks the z3 SMT solver if the formula is SATISFIABLE *)
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

(** Asks the z3 SMT solver for a kripke model if the formula is SATISFIABLE *)
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
		(* p_out !res out; *)
		!res;
	end

(*--------------------------------------------------------*)
(*                 Coeur du programme                     *)
(*--------------------------------------------------------*)


    (** This function solve the formula *)
let solve fo a =

	let ic,oc = U.open_process "z3 -in"
 	and out = if A.mem "--get-log" Sys.argv then Some stdout else None
	and res = ref true
	in begin
		init oc out;
  	p_axiom oc out a;
		p_prop oc out fo;
		p_for oc out fo;
  	(match check_sat ic oc out with
		| UNSAT ->
			let s = "s UNSATISFIABLE\n"
			in begin
				fpf "%s" s;
				res := false;
			end
		| SAT ->
			let s = "s SATISFIABLE\n"
                        (* and m = get_model ic oc out *)
			in begin
     		fpf "%s" s;
       	if Array.mem "--get-model" Sys.argv then
          let m = get_model ic oc out in
          print_string m;
			end);
		U.close_process (ic,oc) |> ignore;
		!res;
	end
