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



type thetfor = (string*string,unit) H.t
(* représente \Theta_\forall *)

type thetsym = (string*string,unit) H.t
(* représente \Theta_sym  
/!\ : comme l'ordre ne compte pas (i.e. on stocke des ensembles de string
et non des couples ), on rentre les strings en ordre croissant 
(+ facile pour retrouver) *)

type thetrans = (string*string*string,unit) H.t

type theteuc = (string*(string*string),unit) H.t
(*
Même remarque que pour thetsym pour l'es deux dernbiers éléments de la
clef
*)

type thetfonc = (string,string) H.t













