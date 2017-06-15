(*########################################################*)
(*       Pretty Printer pour FO & Modal Formules          *)
(*########################################################*)


module S = String
module P = Printf

module FO = Ast_fo.FO
module M = Ast_modal


let spf = P.sprintf

let fpf  = P.printf

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

(*
(*--------------------------------------------------------*)
(*          Pour les éléments du module Solve            *)
(*--------------------------------------------------------*)
let aux_env env =
	let res = ref "" in
	let aux k f = 
		res := !res ^(spf "%s -> %s \n" k (aux_fo f))
	in begin
		So.Smap.iter aux env;
		!res;
	end

let print_env env = 
	fpf "%s \n" (aux_env env)
*)
