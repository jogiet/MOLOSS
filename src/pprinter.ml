(*########################################################*)
(*       Pretty Printer pour FO & Modal Formules          *)
(*########################################################*)

(**
   This module contains function to get string of formula used
   in MOLOSS (Modal logic, FO, BFO) and to pretty-print them.
   *)

module S = String
module F = Format

module FO = Ast_fo.FO
module BFO = Ast_fo.BFO
module M = Ast_modal
module P = Ast_proof.P
module H = Hashtbl

    (**/**)
let spf = Printf.sprintf
let fpf = Printf.printf
let add_parenthesis s = 
  spf "(%s)" s
    (**/**)


(*--------------------------------------------------------*)
(*          Pour les formules de logique modale           *)
(*--------------------------------------------------------*)

  (** Does not print the formula,
      but returns a string representing the formula *)
let aux_m f = 
  (* Some basics functions for pretty-printing *)
  let rec aux_atom f = 
    match f with
    | M.True -> "true"
    | M.False -> "false"
    | M.Atom p -> spf "p%d" p
    | M.Not f0 -> spf "~%s" (aux_atom f0)
    | M.Conj _ -> aux_conj f |> add_parenthesis
    | M.Dij _ -> aux_dij f |> add_parenthesis
    | M.Boxe _ | M.Diamond _ -> aux_boxe_diamond f |> add_parenthesis
    | M.Impl _ -> aux_impl f |> add_parenthesis
  and aux_conj = function 
  | M.Conj (f1,f2) -> 
    spf "%s & %s" (aux_conj f1) (aux_conj f2)
  | _ as f -> aux_atom f
  and aux_dij = function
  | M.Dij (f1,f2) ->
    spf "%s | %s" (aux_dij f1) (aux_dij f2)
  | M.Conj _ as f -> aux_conj f
  | _ as f -> aux_atom f 
  and aux_boxe_diamond = function
  | M.Boxe f -> spf "[r1] %s" (aux_boxe_diamond f)
  | M.Diamond f -> spf "<r1> %s" (aux_boxe_diamond f)
  | _ as f -> aux_atom f
  and aux_impl f = 
  match f with
  | M.Impl (f1,f2) -> 
    spf "%s -> %s" (aux_atom f1) (aux_impl f2)
  | _ -> aux_atom f
in 
match f with
| M.Atom _ | M.Not _ | M.True | M.False -> aux_atom f
| M.Conj _ -> aux_conj f
| M.Dij _ -> aux_dij f
| M.Impl _  -> aux_impl f
| M.Boxe _ | M.Diamond _ -> aux_boxe_diamond f

  (** Prints the formula on stdout *)
let print_m f =
	fpf "%s \n" (aux_m f)

(*--------------------------------------------------------*)
(*          Pour les formules du premier ordre            *)
(*--------------------------------------------------------*)
  (** Does not print the formula,
      but returns a string representing the formula *)
let rec aux_fo f =
  let rec aux_atom = function
  | FO.Atom (p,x) -> spf "P%d(w%d)" p x
  | FO.Not f -> spf "~ %s" (aux_atom f)
  | FO.Relation (x,y) ->
    spf "w%dRw%d" x y
  | FO.Forall (x,f) ->
    spf "forall w%d, %s" x (aux_atom f)
  | FO.Exists (x,f) ->
    spf "exists w%d, %s" x (aux_atom f)
  | FO.Conj _ as f -> aux_conj f |> add_parenthesis
  | FO.Dij _ as f -> aux_dij f |> add_parenthesis
  and aux_conj = function
  | FO.Conj (f1,f2) -> spf "%s & %s" (aux_conj f1) (aux_conj f2)
  | _ as f -> aux_atom f 
  and aux_dij = function
  | FO.Dij (f1,f2) ->spf "%s | %s" (aux_dij f1) (aux_dij f2)
  | FO.Conj _ as f -> aux_conj f
  | _ as f -> aux_atom f
in aux_dij f


  (** Prints the formula on stdout *)
let print_fo f =
	fpf "%s \n" (aux_fo f)

  (** Does not print the formula,
      but returns a string representing the formula *)
let rec aux_bfo f =
  let rec aux_atom = function
  | BFO.Atom x -> spf "v%d" x
  | BFO.Not f -> spf "~ %s" (aux_atom f)
  | BFO.Conj _ as f -> aux_conj f |> add_parenthesis
  | BFO.Dij _ as f -> aux_dij f |> add_parenthesis
  and aux_conj = function
  | BFO.Conj (f1,f2) -> spf "%s & %s" (aux_conj f1) (aux_conj f2)
  | _ as f -> aux_atom f
  and aux_dij = function
  | BFO.Dij (f1,f2) -> spf "%s | %s" (aux_dij f1) (aux_dij f2)
  | BFO.Conj _ as f -> aux_conj f
  | _ as f -> aux_atom f
in aux_dij f

(** Prints the formula on stdout *)
let print_bfo f =
	fpf "%s \n" (aux_bfo f)

(*--------------------------------------------------------*)
(*            Pour les formules et les preuves            *)
(*--------------------------------------------------------*)
  (** Returns an offset made of spaces *)
let p_off n =
	S.make n ' '


  (** Does not print the formula,
      but returns a string representing the formula
      @deprecated We don't handle Z3 proofs yet
  *)
let rec aux_fp env = function
| P.TRUE -> "true"
| P.FALSE -> "false"
| P.Reff _ -> assert false
| P.Atom a -> aux_fo (H.find env a)
| P.Not f -> spf "not (%s)" (aux_fp env f)
| P.Impl (f1,f2) -> spf "(%s) => (%s)" (aux_fp env f1) (aux_fp env f2)
| P.Equiv (f1,f2) -> spf "(%s) <=> (%s)" (aux_fp env f1) (aux_fp env f2)
| P.Conj (f1,f2) -> spf "(%s) and (%s)" (aux_fp env f1) (aux_fp env f2)
| P.Dij (f1,f2) -> spf "(%s) or (%s)" (aux_fp env f1) (aux_fp env f2)
| P.Equal (f1,f2) -> spf "(%s) = (%s)" (aux_fp env f1) (aux_fp env f2)

  (** Does not print the proof,
      but returns a string representing the proof
      @deprecated We don't handle Z3 proofs yet
  *)
let rec aux_pp env off = function
| P.Refp _ -> assert false
| P.Axiom f -> spf "%s%s\n%s%s\n"
	(p_off off)  (aux_fp env f)
	(p_off off)  "\027[94m|AXIOM|\027[0m"
| P.Asserted f -> spf "%s%s\n%s%s\n"
	(p_off off)  (aux_fp  env f)
	(p_off off) "\027[94m|ASSERTED|\027[0m"
| P.AndElim (p,f) -> spf "%s%s\n%s%s\n%s>\n%s"
	(p_off off) (aux_fp env  f)
	(p_off off) "\027[94m|AndELIM|\027[0m"
		(p_off (off+8)) (* > *)
			(aux_pp env (off+9) p)
| P.MP (p1,p2,f) -> spf "%s%s\n%s%s\n%s>\n%s%s>\n%s"
	(p_off off) (aux_fp env f)
	(p_off off) "\027[94m|MP|\027[0m"
		(p_off (off+3)) (* > *)
			(aux_pp env (off+4) p1)
		(p_off (off+3)) (* > *)
			(aux_pp env (off+4) p2)
| P.Rewrite (f) ->
begin
	match f with
	| P.Equal (f1,f2) ->
		spf "%s%s\n%s<->\n%s%s\n%s%s\n"
	(p_off off) (aux_fp env f1)
	(p_off off) (* <-> *)
	(p_off off) (aux_fp env f2)
	(p_off off) "\027[94m|REWRITE|\027[0m"
	| _ -> assert false
end
| P.Unit (p,pl,f) -> spf "%s%s\n%s%s\n%s>\n%s%s"
	(p_off off) (aux_fp env f)
	(p_off off) "\027[94m|UNIT|\027[0m"
		(p_off (off+5)) (* > *)
			(aux_pp env (off+6) p)
		(let res = ref "" in
		 let aux p  =
			res := spf "%s%s>\n%s" !res (p_off (off+5)) (aux_pp env (off+6) p)
		 in begin
		 	List.iter aux pl;
			!res;
		 end)
| P.Monotonicity (pl,f) -> spf "%s%s\n%s%s\n%s"
	(p_off off) (aux_fp env f)
	(p_off off) "\027[94m|MONOTONICITY|\027[0m"
		(let res = ref "" in
		 let aux p  =
			res := spf "%s%s>\n%s" !res (p_off (off+5)) (aux_pp env (off+6) p)
		 in begin
		 	List.iter aux pl;
			!res;
		 end)
| P.Trans (p,q,r) -> spf "%s%s\n%s%s\n%s>\n%s%s>\n%s"
	(p_off off) (aux_fp env r)
	(p_off off) "\027[94m|TRANSITIVITY|\027[0m"
	(p_off (off +5))
		(aux_pp env (off+6) p)
	(p_off (off +5))
		(aux_pp env (off+6) q)
| P.Hyp f -> spf "%s%s\n%s%s\n"
	(p_off off) (aux_fp env f)
	(p_off off) "\027[94m|HYPOTHESIS|\027[0m"
| P.Lemma (p,f) -> spf "%s%s\n%s%s\n%s>\n%s"
	(p_off off) (aux_fp env f)
	(p_off off) "\027[94m|LEMMA|\027[0m"
		(p_off (off+5)) (* > *)
		  (aux_pp env (off +6) p)



  (** Prints the proof on stdout
      @deprecated We don't handle Z3 proofs yet
  *)
let print_proof env p =
	fpf "%s" (aux_pp env 0 p)
