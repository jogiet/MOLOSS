(*########################################################*)
(*       Pretty Printer pour FO & Modal Formules          *)
(*########################################################*)


module S = String
module F = Format

module FO = Ast_fo.FO
module M = Ast_modal
module P = Ast_proof.P
module H = Hashtbl

let spf = Printf.sprintf

let fpf  = Printf.printf



(*--------------------------------------------------------*)
(*          Pour les formules de logique modale           *)
(*--------------------------------------------------------*)

let rec aux_m = function
(* Renvoie seulement la chaine de la formule *)
| M.Atom p -> p
| M.Not f -> spf "~ (%s)" (aux_m f)
| M.Conj (f1,f2) -> spf "(%s) & (%s)" (aux_m f1) (aux_m f2)
| M.Dij (f1,f2) -> spf "(%s) || (%s)" (aux_m f1) (aux_m f2)
| M.Impl (f1,f2) -> spf "(%s) => (%s)" (aux_m f1) (aux_m f2)
| M.Boxe f -> spf "[] (%s)" (aux_m f) 
| M.Diamond f -> spf "<> (%s)" (aux_m f)

let print_m f =
(* print la formule sur stdout *)
	fpf "%s \n" (aux_m f) 

(*--------------------------------------------------------*)
(*          Pour les formules du premier ordre            *)
(*--------------------------------------------------------*)
let rec aux_fo = function
(* Renvoie seulement la chaine de la formule *)
| FO.Atom (p,x) -> spf "%s(%s)" p x
| FO.Not f -> spf "~ %s" (aux_fo f)
| FO.Conj (f1,f2) -> spf "(%s) & (%s)" (aux_fo f1) (aux_fo f2)
| FO.Dij (f1,f2) -> spf "(%s) | (%s)" (aux_fo f1) (aux_fo f2)
| FO.Relation (x,y) -> spf "%sR%s" x y
| FO.Forall (x,f)-> 
	let quant = "forall"
	in spf "%s %s, %s" quant x (aux_fo f)
| FO.Exists (x,f)-> 
	let quant = "exists"
	in spf "%s %s, %s" quant x (aux_fo f)

let print_fo f =
(* print la formule sur stdout *)
	fpf "%s \n" (aux_fo f)

(*--------------------------------------------------------*)
(*            Pour les formules et les preuves            *)
(*--------------------------------------------------------*)
let p_off n = 
	S.make n ' '


let rec aux_fp env = function
| P.TRUE -> "true"
| P.FALSE -> "false"
| P.Reff _ -> assert false
| P.Atom a -> aux_fo (H.find env a)^(spf "--> %s" a)
| P.Not f -> spf "not (%s)" (aux_fp env f)
| P.Impl (f1,f2) -> spf "(%s) => (%s)" (aux_fp env f1) (aux_fp env f2)
| P.Equiv (f1,f2) -> spf "(%s) <=> (%s)" (aux_fp env f1) (aux_fp env f2)
| P.Conj (f1,f2) -> spf "(%s) and (%s)" (aux_fp env f1) (aux_fp env f2)
| P.Dij (f1,f2) -> spf "(%s) or (%s)" (aux_fp env f1) (aux_fp env f2)
| P.Equal (f1,f2) -> spf "(%s) = (%s)" (aux_fp env f1) (aux_fp env f2)

let rec aux_pp env off = function
| P.Refp _ -> assert false
| P.Axiom f -> spf "%s%s\n%s|AXIOM|\n"
	(p_off off)  (aux_fp env f) (p_off off)
| P.Asserted f -> spf "%s%s\n%s|ASSERTED|\n"
	(p_off off)  (aux_fp  env f) (p_off off)
| P.AndElim (p,f) -> spf "%s%s\n%s|AndElim|\n%s>\n%s"
	(p_off off) (aux_fp env  f) 
	(p_off off) (* |AndElim| *)
		(p_off (off+8)) (* > *) 
			(aux_pp env (off+9) p)
| P.MP (p1,p2,f) -> spf "%s%s\n%s|MP|\n%s>\n%s%s>\n%s"
	(p_off off) (aux_fp env f) 
	(p_off off) (* |MP| *) 
		(p_off (off+3)) (* > *) 
			(aux_pp env (off+4) p1)
		(p_off (off+3)) (* > *) 
			(aux_pp env (off+4) p2)
| P.Rewrite (f1,f2) -> spf "%s%s\n%s<->\n%s%s\n%s|REWRITE|\n"
	(p_off off) (aux_fp env f1)
	(p_off off)
	(p_off off) (aux_fp env f2)
	(p_off off)
| P.Unit (p,pl,f) -> spf "%s%s\n%s|UNIT|\n%s>\n%s%s"
	(p_off off) (aux_fp env f)
	(p_off off)
		(p_off (off+5)) (* > *) 
			(aux_pp env (off+6) p)
		(let res = ref "" in
		 let aux p  = 
			res := spf "%s%s>\n%s" !res (p_off (off+5)) (aux_pp env (off+6) p)
		 in begin
		 	List.iter aux pl;
			!res;
		 end)
| P.Monotonicity (pl,f) -> spf "%s%s\n%s|MONOTONICITY|\n%s"
	(p_off off) (aux_fp env f)
	(p_off off)
		(let res = ref "" in
		 let aux p  = 
			res := spf "%s%s>\n%s" !res (p_off (off+5)) (aux_pp env (off+6) p)
		 in begin
		 	List.iter aux pl;
			!res;
		 end)

let print_proof env p = 
	fpf "%s" (aux_pp env 0 p)
