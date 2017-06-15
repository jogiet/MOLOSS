(*########################################################*)
(*              Procédures de décisions                   *)
(*########################################################*)

module FO = Ast_fo.FO
module BFO = Ast_fo.BFO
module PP = Pprinter



(*--------------------------------------------------------*)
(*               Quelques modules utils                   *)
(*--------------------------------------------------------*)

(*
On essaye d'avoir une idée de l'implem à utiliser.
-> Comme on ne bactrack pas mais on fiat beaucoup d'ajouts, on utlise
une structure mutable : des tables de hash
TODO : trouver une structure motable équivalente aus Set !!!
*)

module H = Hashtbl

type env = (string,FO.formula) H.t 
(* Un atome de BFO (de type string) est bind à la formule qu'il encadre *)

type thetex = (string,unit) H.t
(* représente \Theta_\exists *)

module DS : H.HashedType = struct
	type t = string*string

	let equal (s11,s12) (s21,s22) = 
		((s11 = s21) && (s12 = s22)) || ((s11 = s22) && (s12= s21))
	
	let hash (s1,s2) = 
		assert false

end

module DSH = Hashtbl.Make(DS)

type thetfor = unit DSH.t
(* représente \Theta_\forall *)

type thetsim = SSSet.t
(* représente \Theta_sym *)

















