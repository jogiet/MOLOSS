(*########################################################*)
(*              Procédures de décisions                   *)
(*########################################################*)

(**These are the decision procedures for the basic modal logic (see
Pascal Fontaine et al.) and for the epistemic logic *)

module GetDecide (A : Sign.DecideArg) : Sign.Decide = struct

module FO = Ast_fo.FO
module BFO = Ast_fo.BFO
open Ast_fo
let fpf = Printf.printf

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
module PP = Pprinter




type env = (string,FO.formula) H.t
(** An atom from BFO is bind to the FO formula it represents *)
(* Un atome de BFO (de type string) est bind à la formule qu'il encadre *)

type thetex = (string,unit) H.t
(** Represents the set \Theta_\exists *)
(* représente \Theta_\exists *)



type thetfor = (string*int,unit) H.t
(** Represents the set \Theta_\forall *)
(* représente \Theta_\forall *)

type thetsym = (int*int,unit) H.t
(** Represents the set \Theta_\forall.
Warning : since we store sets, worlds (i.e. strings) are put in
alphabetical order *)

type thetrans = (int*int*int,unit) H.t
(** Represents the set \Theta_T *)

type theteuc = (int*(int*int),unit) H.t
(** Represents the set \Theta_e.
Same remark for thetsym *)
(*
Même remarque que pour thetsym pour les deux derniers éléments de la
clef
*)

type thetfonc = (int,int) H.t
(** Represents the set \Theta_f *)

type config =
	{mutable cardw : int;
	 mutable w : int list;
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

let new_config () =
  let config =
    {cardw = 1;
     w = [0];
     env = H.create 10;
     s = ();
     exists = H.create 10;
     forall = H.create 10;
     reflex = H.create 10;
     sym = H.create 10;
     trans = H.create 10;

     euc = H.create 10;
     fonc = H.create 10}
  in begin
    config;
  end

type model = (string*bool) list
(** Type of a model returned by SMT-solver  *)


(** returns :
    - the list of the boxed formula corresponding to the FO formula
    - the variables in the boxed formula

    and add the formula in the config by side effect. *)
let rec init (config : config) = function
| [] ->
		[],[]
| f::q ->
	let f_box,new_var = abs config.env f in
	let f_rest,new_var_rest = init config q in
		f_box::f_rest, new_var@new_var_rest


exception Found of (string list*BFO.formula)
(** When a decision prcedures is applied, it raises an exception with :
    - the new boxed atoms,
    - the new formula *)

(** When \exists_soft applies, it raises an exception with :
    - a list of atoms for the new formula,
    - a new FO formula,
    - a list of atoms for the "soft" formula,
    - the soft formula,
    - its weight *)
exception SoftFound of
	(string list*
	 BFO.formula*
	 string list*
	 BFO.formula*
	 int)


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
		config.cardw <- config.cardw + 1;
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
	(* D'abord on extrait les realtions du modèle *)
	let rec make_rel config  = function
	| [] -> []
	| (eps,b)::q when b ->
	begin
		match H.find config.env eps with
		| FO.Relation (x,y) -> (x,y)::(make_rel config q)
		| _ -> make_rel config q
	end
	| _::q -> make_rel config q
	(* Puis on parcourt le modèle pour trouver un epsilon qui convient *)
	and aux config rel = function
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
  (* On Combine les deux *)
	in aux config (make_rel config model) model


(*  ===========>  Réflexivité  <=========== *)
(** Decision procedure for the reflexivity rule (use the modal axiom :
W\R w, is implicit in the frame *)
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
			config.cardw <- config.cardw + 1;
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





(** Returns the list of the decision procdures for the formula  *)
let axiom_to_dec_proc axiom =
  let rec aux = function
    | [] -> []
    | "-S"::q | "-M"::q | "-boxeM"::q ->
      if L.mem "-5" axiom then
        reflexiv::(aux q)
      else
        reflexiv::(aux q)
    | "-K"::q -> aux q
    | "-4"::q -> transitivity::(aux q)
    | "-B"::q -> symmetric::(aux q)
    | "-5"::q -> euclidean::(aux q)
    | "-CD"::q -> functionnal::(aux q)
    | t::q ->
    begin
      if String.length t <= 1
      then begin
        Printf.printf "argument inconnu : %s\n" t;
        exit 0
      end
      else if String.get t 0 = '-' && String.get t 1 != '-'
      then
      begin
        fpf "Axiome inconne : %s \n" t;
        exit 1
      end
      else
        aux q
    end
  in let res = aux axiom in
  if L.mem  "-CD" axiom  || L.mem "-boxeM" axiom then
    res@[forall]
  else if (L.mem "-4" axiom || L.mem "-5" axiom || L.mem "--soft" axiom)
    && not (L.mem "--soft-ignore" axiom) then
    res@[forall;softexist]
  else
    res@[forall;exist]



let print_model config model =
  (** prints the model in the standard output *)
  let prop = H.create 42
  and cardProp = ref 0 in
  let relation = ref [] in
  let aux (k,b) =
    let f = H.find config.env k in
    match f with
    | FO.Relation (w1,w2) ->
      if b then relation := (w1,w2)::!relation
    | FO.Atom (p,w)   ->
    begin
      if p > !cardProp then cardProp := p;
      H.add prop (p,w) b;
    end
    | FO.Not (FO.Atom (p,w)) ->
    begin
      if p > !cardProp then cardProp := p;
      H.add prop (p,w) (not b);
    end
    | FO.Exists _ | FO.Forall _ -> ()
    |  _ -> assert false
  and print_world wi =
    print_string "v ";
    for pj = 1 to !cardProp do
      if H.mem prop (pj,wi) then
        if H.find prop (pj,wi) then
          fpf "%d " pj
        else
          fpf "-%d " pj
      else
        fpf "-%d " pj
    done;
    fpf "0\n"
  in begin
    List.iter aux model;
    fpf "c #propositions #worlds #relations #edges\n";
    fpf "v %d %d %d %d\n" !cardProp config.cardw 1 (List.length !relation);
    for wi = 0 to (config.cardw -1)  do print_world wi done;
    List.iter (fun (w1,w2) -> fpf "v r1 w%d w%d\n" w1 w2) !relation
  end

  (** the list of decision procedures used to solve a formula  *)
let decisions = axiom_to_dec_proc A.argument

  (** a function that applies all decision procedures  *)
let decide config m = L.iter (fun d_proc -> d_proc config m) decisions

let printDecVar v config =
  fpf "c #%s -> %s\n" v (PP.aux_fo (Hashtbl.find config.env v))

let printAssert bfo config =
  fpf "c \027[92massert\027[0m : %s\n" (PP.aux_bfo bfo)

let printAssertSoft bfoSoft config =
  fpf "c \027[94m assert-soft\027[0m : %s\n" (PP.aux_bfo bfoSoft)

end
