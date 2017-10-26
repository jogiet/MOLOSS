(*########################################################*)
(*              Procédures de décisions                   *)
(*########################################################*)

(**These are the decision procedures for the basic modal logic (see
Pascal Fontaine et al.) and for the epistemic logic *)

module FO = Ast_fo.FO
module BFO = Ast_fo.BFO
open Ast_fo


(*--------------------------------------------------------*)
(*               Quelques modules utils                   *)
(*--------------------------------------------------------*)

(*
On essaye d'avoir une idée de l'implem à utiliser.
-> Comme on ne bactrack pas mais on fiat beaucoup d'ajouts, on utlise
une structure mutable : des tables de hash
TODO : trouver une structure mutable équivalente aux Set !!
=> réponse une table de hash ('a,unit) ... pas plus opti ?
*)

module H = Hashtbl
module L = List

type env = (string,FO.formula) H.t
(** An atom from BFO is bind to the FO formula it represents *)
(* Un atome de BFO (de type string) est bind à la formule qu'il encadre *)

type thetex = (string,unit) H.t
(** Represents the ensemble \Theta_\exists *)
(* représente \Theta_\exists *)



type thetfor = (string*string,unit) H.t
(** Represents the ensemble \Theta_\forall *)
(* représente \Theta_\forall *)

type thetsym = (string*string,unit) H.t
(** Represents the ensemble \Theta_\forall.
Warning : since we store ensembles, worlds (i.e. strings) are put in
alphabetical order *)

type thetrans = (string*string*string,unit) H.t
(** Represents the ensemble \Theta_T *)

type theteuc = (string*(string*string),unit) H.t
(** Represents the ensemble \Theta_e.
Same remark for thetsym *)
(*
Même remarque que pour thetsym pour les deux derniers éléments de la
clef
*)

type thetfonc = (string,string) H.t
(** Represents the ensemble \Theta_f *)

type config =
	{mutable cardw : int;
	 mutable w : string list;
	 env : env;
	 s : unit; (* a priori, S est inutile algiorithmiquement *)
	 exists : thetex;
	 forall : thetfor;
	 reflex : thetex;
	 sym : thetsym;
	 trans : thetrans;
	 euc : theteuc;
	 fonc : thetfonc}
(** The type for a configuration *)

exception Found of (string list*BFO.formula)
(** When a decision prcedures is applied, it raises an exception whith
the new boxed atoms and the new formula *)
exception SoftFound of
(** When \exists_{soft} applies, it raises an exception with a list of
axioms for the new formula, a new FO formula, a list of atoms for the
"soft" formula, the soft formula and its weight*)
	(string list*
	 BFO.formula*
	 string list*
	 BFO.formula*
	 int)

(*
Le résultat des fonctions de décision se fera par des exceptions ...
Le type renvoyé par les fonctions est unit (parce que L.iter ^^ )
*)

(*
Une exception prend comme arguement :
-> La liste des nouvelle variables à rajouter au modèle
	(Elles correspondent aux nouvelles variables des formules encadrées
	(ci-dessous
-> Une nouvelle formule à rajouter
Dans le cas de Soft, il y a en plus :
-> Une formule à mettre en assert soft
-> et le poids de la formule
Les fonctions de décisions changent l'environnement par effet de bord
et la config de manière générale
*)

type init_flag = | Reflexiv
(** Type for initialisation, @deprecated : not necessary with the new
reflexivity decision procedure *)

(*--------------------------------------------------------*)
(*               Fonctions de décision                    *)
(*--------------------------------------------------------*)


(*  ===========>  règle exists  <=========== *)
(** Decision procedure for the \exists rule *)
let rec exist config = function
| [] -> ()
| (eps,b)::q when b ->
begin
if H.mem config.exists eps then
	exist config q
else
	let f = H.find config.env eps in
	match f with
	| FO.Exists (y,FO.Conj (FO.Relation (c,y0),fy)) ->
	let w = get_fw () in
	let fd =
		FO.Dij (FO.Not f,
				FO.Conj (FO.Relation (c,w),FO.changefv w fy)) in
	let f_tot,new_var = abs config.env fd
	in begin
		config.w <- w::config.w;
		H.add config.exists eps () ;
		raise (Found (new_var,f_tot));
	end
	| _ -> exist config q
end
| _::q -> exist config q


(*  ===========>  règle forall  <=========== *)
(** Decision procedure for the \forall rule *)
let rec forall config model  =
	let rec make_rel config  = function
	(* D'abord on extrait les realtions du modèle *)
	| [] -> []
	| (eps,b)::q when b ->
	begin
		match H.find config.env eps with
		| FO.Relation (x,y) -> (x,y)::(make_rel config q)
		| _ -> make_rel config q
	end
	| _::q -> make_rel config q
	and aux config rel = function
	(* Puis on parcourt le modèle pour trouver un epsilon qui convient *)
	| [] -> ()
	| (eps,b)::q when b ->
	begin
		let f = H.find config.env eps in
		match f with
		| FO.Forall (y,FO.Dij (FO.Not FO.Relation (c,y0),fy)) ->
		begin
			let aux_find (c1,d1) =
				c = c1 && not (H.mem config.forall (eps,d1))
			in
			try
				let d = snd (L.find aux_find rel) in
				let fd =
					FO.Dij (FO.Not f,
						FO.Dij (FO.Not (FO.Relation (c,d)),FO.changefv d
						fy)) in
				let f_tot,new_var = abs config.env fd
				in begin
					H.add config.forall (eps,d) ();
					raise (Found (new_var,f_tot));
				end
			with Not_found -> aux config rel q
		end
		| _ -> aux config rel q
	end
	| _::q ->  aux config rel q
	in aux config (make_rel config model) model

(*
(*  ===========>  Réflexivité  <=========== *)
let rec reflexiv config = function
(* Il s'agit d'une réécriture de la fonction exists *)
| [] -> ()
| (eps,b)::q when b ->
begin
if H.mem config.exists eps then
	reflexiv config q
else
	let f = H.find config.env eps in
	match f with
	| FO.Exists (y,FO.Conj (FO.Relation (c,y0),fy)) ->
	let w = get_fw () in
	let fd =
	FO.Dij (FO.Not f,
			FO.Conj (FO.Conj (FO.Relation (c,w),
				          	  FO.Relation (w,w)),
			     	 FO.changefv w fy)) in
	let f_tot,new_var = abs config.env fd
	in begin
		config.w <- w::config.w;
		H.add config.exists eps () ;
		raise (Found (new_var,f_tot));
	end
	| _ -> reflexiv config q
end
| _::q -> reflexiv config q
(*This is the former implementation of the Reflexivity rule
it's based on the axiom's FO version*)
*)

(** Decision procedure for the reflexivity rule (use the modal axiom :
W\R w, is implictt in the frame *)
let reflexiv config model =
	let aux (eps,b) =
	(* renvoie true si le couple est bon pour trigger *)
		b &&
		begin
		match H.find config.env eps with
		| FO.Forall (_,(FO.Dij (FO.Not (FO.Relation (c,_)),f)) )
		when not (H.mem config.reflex eps) ->
		begin
			H.add config.reflex eps ();
			H.add config.forall (eps,c) ();
			true;
		end
		| _ -> false
		end
	in
 	match L.filter aux model with
	(*  On recherche ensuite tous les couple qui triggent *)
	| [] -> ()
	| (eps,_)::q ->
	let fd = L.fold_left
	(fun f0 (eps,_) ->
		let f = H.find config.env eps in
		match f with
		| FO.Forall (_,FO.Dij (FO.Not (FO.Relation (w,_)),fi)) ->
			FO.Conj (f0,
		  			 FO.Dij (FO.Not f,
		 		 		     FO.changefv w fi))
		| _ -> assert false)
	(let f0 = H.find config.env eps in
	 match f0 with
	 | FO.Forall (_,FO.Dij (FO.Not (FO.Relation (w,_)),fi)) ->
	 	FO.Dij (FO.Not f0,
			     FO.changefv w fi)
	 | _ -> assert false)
	q in
	let f_tot,new_var = abs config.env fd
	in begin
		raise (Found (new_var,f_tot));
	end




(*  ===========>  Fonctionnelle  <=========== *)
(** Decision procedure for the fonctionnal rule *)
let rec functionnal config = function
| [] -> ()
| (eps,b)::q when b ->
begin
if H.mem config.exists eps then
	functionnal config q
else
	let f = H.find config.env eps in
	match f with
	| FO.Exists (y,FO.Conj (FO.Relation (c,y0),fy)) ->
	let w =
		(* c'est ici la modification *)
		if H.mem config.fonc c then
			H.find config.fonc c
		else
		let res = get_fw ()
		in begin
			config.w <- res::config.w;
			H.add config.fonc c res;
			res;
		end
	in
	let fd =
		FO.Dij (FO.Not f,
				FO.Conj (FO.Relation (c,w),FO.changefv w fy)) in
	let f_tot,new_var = abs config.env fd
	in begin
		H.add config.exists eps () ;
		raise (Found (new_var,f_tot));
	end
	| _ -> functionnal config q
end
| _::q -> functionnal config q



(*  ===========>  Symétrie  <=========== *)
(** Decision procedure for the symmetrical rule *)
let rec symmetric config = function
| [] -> ()
| (eps,b)::q  when b ->
begin
	let f = H.find config.env eps in
	match f with
	| FO.Relation (x,y) ->
		let x, y = if x <= y then (x,y)
							 else (y,x)
		in
		if H.mem config.sym (x,y) then
			symmetric config q
		else
			let xy = FO.Relation (x,y)
			and yx = FO.Relation (y,x) in
			let f = FO.Dij (FO.Conj (xy,yx),
							FO.Conj (FO.Not xy, FO.Not yx))
			in
			let f_tot,new_var = abs config.env f
			in begin
				H.add config.sym (x,y) ();
				raise (Found (new_var,f_tot));
			end
	| _ -> symmetric config q
end
| _::q -> symmetric config q


(*  ===========> Transitivité  <=========== *)
(** Decision procedure for the transitivity rule *)
let transitivity config model =
	let rec aux rel = function
	| [] -> ()
	| (eps,b)::q when b ->
	begin
		match H.find config.env eps with
		| FO.Relation (x,y) ->
		begin
			let aux_pred (u,v) = (* u -> v = x -> y *)
				v = x && not (H.mem config.trans (u,x,y))
			and aux_succ (u,v) = (* x -> y = u -> v *)
				y = u && not (H.mem config.trans (x,y,v))
			in
			match L.filter aux_pred rel with
			| (u,_)::_ -> (* u -> x -> y *)
				let f =
					FO.Dij (FO.Dij (FO.Not (FO.Relation (u,x)),
									FO.Not (FO.Relation (x,y))),
							FO.Relation (u,y))
				in
				let f_tot,new_var = abs config.env f
				in begin
					H.add config.trans (u,x,y) ();
					raise (Found (new_var,f_tot));
				end
			| [] ->
			match L.filter aux_succ rel with
			| (_,v)::_ -> (* x -> y -> v *)
				let f =
					FO.Dij (FO.Dij (FO.Not (FO.Relation (x,y)),
									FO.Not (FO.Relation (y,v))),
							FO.Relation (x,v))
				in
				let f_tot,new_var = abs config.env f
				in begin
					H.add config.trans (x,y,v) ();
					raise (Found (new_var,f_tot));
				end
			| [] -> aux ((x,y)::rel) q
		end
		| _ -> aux rel q
	end
	| _::q -> aux rel q
	in
	aux [] model

(* ===========> Euclidienne  <=========== *)
(** Decision procedure for the euclidian rule *)
let euclidean config model =
	let rec aux rel = function
	| [] -> ()
	| (eps,b)::q when b ->
	begin
		match H.find config.env eps with
		| FO.Relation (x,u) ->
		begin
			let aux_find (x0,v) =
				x = x0 && not (H.mem config.euc (x,if u <= v then (u,v)
															 else (v,u)))
			in match L.filter aux_find rel with
			| (_,v)::_ ->
				let f =
					FO.Dij (FO.Dij (FO.Not (FO.Relation (x,u)),
									FO.Not (FO.Relation (x,v))),
							FO.Conj (FO.Relation (u,v),
									 FO.Relation (v,u)))
				in
				let f_tot,new_var = abs config.env f
				in begin
					H.add
						config.euc
						(x,if u <= v then (u,v)
									 else (v,u))
						();
					raise (Found (new_var,f_tot));
				end
			| [] -> aux ((x,u)::rel) q
		end
		| _ -> aux rel q
	end
	| _::q -> aux rel q
	in
	aux [] model




(*--------------------------------------------------------*)
(*                  Contrer l'explosion                   *)
(*--------------------------------------------------------*)


(* ===========> règle exists <=========== *)
(** Decision procedure for the \exists_soft rule *)
let rec softexist config = function
| [] -> ()
| (eps,b)::q when b ->
begin
if H.mem config.exists eps then
	softexist config q
else
	let f = H.find config.env eps in
	match f with
	| FO.Exists (y,FO.Conj (FO.Relation (c,y0),fy)) ->
	let w0 = get_fw () in
	let auxd f w =
		FO.Dij (f,
				FO.Conj (FO.Relation (c,w),
						 FO.changefv w fy))
	and auxg f w =
		FO.Conj (f,
				 FO.Not (FO.Relation (w,w0))) in
	let fd =
		FO.Dij (FO.Not f,
				L.fold_left
					auxd
					(FO.Conj (FO.Relation (c,w0),
				 			  FO.changefv w0 fy))
						config.w )
	and fg =
		match config.w with
		| [] -> assert false (* on met au moins "w" *)
		| t::q -> L.fold_left
			auxg
			(FO.Not (FO.Relation (t,w0)))
			q
	in
	let fd_tot,new_var1 = abs config.env fd
	and fg_tot,new_var2 = abs config.env fg
	in begin
		config.cardw <- 1+config.cardw;
		H.add config.exists eps () ;
		config.w <- w0::config.w;
		raise (SoftFound (new_var1,fd_tot,new_var2,fg_tot,config.cardw));
	end
	| _ -> softexist config q
end
| _::q -> softexist config q



(* ===========>   symetrique   <=========== *)
(** Decision procedure for the symmetric rule with assert-soft
@deprecated : not used *)
let rec softreflexiv config = function
| [] -> ()
| (eps,b)::q when b ->
begin
if H.mem config.exists eps then
	softreflexiv config q
else
	let f = H.find config.env eps in
	match f with
	| FO.Exists (y,FO.Conj (FO.Relation (c,y0),fy)) ->
	let w0 = get_fw () in
	let aux1 f w =
		FO.Dij (f,
				FO.Conj (FO.Conj (FO.Relation (c,w),
								  FO.Relation (w,w)),
						 FO.changefv w fy))
	and aux2 f w =
		FO.Conj (f,
				 FO.Not (FO.Relation (w,w0))) in
	let fd =
		FO.Dij (FO.Not f,
				L.fold_left
					aux1
					(FO.Conj (FO.Conj (FO.Relation (c,w0),
									   FO.Relation (w0,w0)),
				 			  FO.changefv w0 fy))
						config.w )
	and fg =
		match config.w with
		| [] -> assert false (* on met au moins "w" *)
		| t::q -> L.fold_left
			aux2
			(FO.Not (FO.Relation (t,w0)))
			q
	in
	let fd_tot,new_var1 = abs config.env fd
	and fg_tot,new_var2 = abs config.env fg
	in begin
		config.cardw <- 1+config.cardw;
		H.add config.exists eps () ;
		config.w <- w0::config.w;
		raise (SoftFound (new_var1,fd_tot,new_var2,fg_tot,config.cardw));
	end
	| _ -> softreflexiv config q
end
| _::q -> softreflexiv config q
