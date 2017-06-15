(*########################################################*)
(*         AST pour les formules du premier ordre         *)
(*########################################################*)



(*--------------------------------------------------------*)
(*                Formules du 1er ordre                   *)
(*--------------------------------------------------------*)


module Smap = Map.Make(String)
module Sset = Set.Make(String)


module FO  = struct 

type ident = string

type quantifier = 
	| Forall
	| Exists

type formula = 
	| Atom of ident*ident (* Atom (p,x) ->  P(x) *)
	| Not of formula
	| Conj of formula*formula
	| Dij of formula*formula
	| Relation of ident*ident
	(* /!\ a priori : R n'est pas réfléxive *)
	| Exists of ident*formula
	| Forall of ident*formula
	(*
	| Quantified of quantifier*ident*formula
	*)

type t = formula
let compare = compare
(* Pour pouvoir donner une structure de Map *)

(*
les fonctions suivantes servent à gérer les variables libres et
notamment la question des variables quantifiées 
*)

exception NotSameFV of (string*string)

let rec extractfv f =
(* renvoie la liste de variables libres dans f *)
match f with
| Atom (_,v) -> [v]
| Not f -> extractfv f
| Conj (f1,f2) | Dij (f1,f2) ->
	let v1 = extractfv f1
	and v2 = extractfv f2 in
		Sset.elements (Sset.union (Sset.of_list v1) (Sset.of_list v2))
		(* c'est moche mais ca marche *)
| Relation (x,y) ->
	if x = y then
		[x]
	else
		[x;y]
| Exists (new_fv,f) | Forall (new_fv,f) -> 
	List.filter (fun x -> x <> new_fv) (extractfv f) 

(*
let rec equalmodfv f1 f2 = 
(* 
Renvoie vrai si les formuiles sont égales modulo la variable libre qui
peut être différente dans les deux formules 
*)
match f1,f2 with
| Atom (p1,_) ,Atom (p2,_) -> p1 = p2
| Not f1,Not f2 -> equalmodfv f1 f2
| Conj (f11,f12) ,Conj(f21,f22) ->
	(equalmodfv f11 f21) && (equalmodfv f12 f22)
| Dij (f11,f12) , Dij(f21,f22) ->
	(equalmodfv f11 f21) && (equalmodfv f12 f22)
| Relation _ , Relation _ -> assert false 
| Forall (x1,f1) , Forall (x2,f2)->
begin
	match f1,f2 with
	| Dij (_,f1),Dij(_,f2) -> 
	equalmodfv f1 f2
	| _ -> assert false
end
| Exists (x1,f1) , Exists (x2,f2)->
begin
	match f1,f2 with
	| Conj (_,f1),Conj (_,f2) -> 
	equalmodfv f1 f2
	| _ -> assert false
end
| _ -> false
*)

let equalmodvquant f1 f2 =
(*
cette fonction renvoie vrai ssi :
- soit, f1 = f2
- soit f1 et f2 sont des formules quantifiées égales modulo la variable
  quantifiée.
*)
	let rec aux f1 f2 pair = 
		match f1,f2 with
		| Atom (p1,x1),Atom (p2,x2) when p1 = p2 ->
			if (List.mem_assoc x1 pair) then
				(List.assoc x1 pair) = x2
			else
				x1 = x2
		| Not f1, Not f2 ->
			aux f1 f2 pair
		| Conj (f11,f12),Conj (f21,f22)
		| Dij (f11,f12),Dij (f21,f22) ->
			(aux f11 f21 pair) && (aux f12 f22 pair)
		| Relation (x1,y1),Relation (x2,y2) ->
			(	if (List.mem_assoc x1 pair) then
					(List.assoc x1 pair) = x2
				else
					x1 = x2	)
			&&
			(	if (List.mem_assoc y1 pair) then
					(List.assoc y1 pair) = y2
				else
					y1 = y2	)
		| Forall (i1,f1),Forall (i2,f2) 
		| Exists (i1,f1),Exists (i2,f2) ->
			aux f1 f2 ((i1,i2)::(List.filter (fun (x1,x2) -> x1 <> i1 &&
			(x2 <> i2) ) pair))
		| _ -> false
	in aux f1 f2 []
			


let rec changefv fv = function
(* remplace la variable libre dans f par fv *)
| Atom (p,_) -> Atom (p,fv)
| Not f -> Not (changefv fv f)
| Dij (f1,f2) ->
	Dij (changefv fv f1,changefv fv f2)
| Conj (f1,f2) ->
	Conj (changefv fv f1,changefv fv f2)
| Relation _ -> assert false
| Exists (y,Conj (f1,f2)) ->
	if y = fv then
		(* cas très chiant mais impossible *)
		assert false
	else
		Exists (y, (Conj(Relation(fv,y),f2)))
| Forall (y,Dij (f1,f2)) ->
	if y = fv then
		(* cas très chiant mais impossible *)
		assert false
	else
		Forall (y, (Dij(Not ( Relation(fv,y)),f2)))
| _ -> assert false


end


(*--------------------------------------------------------*)
(*           Formules du 1er ordre "encadrées"            *)
(*--------------------------------------------------------*)

module BFO = struct

type atom = string

type formula = 
	| Atom of atom
	| Not of formula
	| Conj of formula *formula
	| Dij of formula *formula

type t = formula 
let compare = compare

	type value = bool

end


(*--------------------------------------------------------*)
(*                  Fonctions utiles                      *)
(*--------------------------------------------------------*)

let spf = Printf.sprintf

let i_w = ref 0
let get_fw () = 
(*
Fonction pour les variables skolemisées de la règle exists
les mondes libres sont de la forme w1,w2,w3...
*)
	begin
		incr i_w;
		spf "w%d" !i_w;
	end

let i_v = ref 0
let get_fv () = 
(*
Fonction pour créer de nouveaux noms pour les formules
*)
	begin
		incr i_v;
		spf "v%d" !i_v;
	end

(*--------------------------------------------------------*)
(*                 Fonctions abs et conc                  *)
(*--------------------------------------------------------*)

type env = FO.formula Smap.t

let rec abs env f = 
(*
transforme f en une formule encadrée et enrichit env si besoin
la fonction abs garantit que deux formules quantifiées égales modulo la
variable quantifiée deviennent égale dans abs et dans l'env
On ne doit donc plus s'en soucier ...
*)
match f with
| FO.Atom _ | FO.Exists _ | FO.Forall _ | FO.Relation _ -> 
	let filter = 
		Smap.filter
			(fun ki fi -> FO.equalmodvquant f fi)
			env
	in
	if (Smap.is_empty filter) then
		let i = get_fv () in
		(BFO.Atom i , Smap.add i f env,[i])
	else
		let i = fst (Smap.choose filter) in
		(BFO.Atom i,env, [])
| FO.Not f -> 
	let bf,new_env,new_var = abs env f in
		(BFO.Not bf ,new_env,new_var)
| FO.Conj (f1,f2) ->
	let bf1,env1,new_var1 = abs env f1 in
	let bf2,env2,new_var2 = abs env1 f2 in
		(BFO.Conj (bf1,bf2),env2,new_var1@new_var2)
| FO.Dij (f1,f2) ->
	let bf1,env1,new_var1 = abs env f1 in
	let bf2,env2,new_var2 = abs env1 f2 in
		(BFO.Dij (bf1,bf2),env2,new_var1@new_var2)

let rec conc env bf = 
match bf with 
| BFO.Atom i -> 
	(Smap.find i env)
| BFO.Not bf ->
	FO.Not (conc env bf)
| BFO.Conj (bf1,bf2) ->
	FO.Conj (conc env bf1, conc env bf2)
| BFO.Dij (bf1,bf2) ->
	FO.Dij (conc env bf1, conc env bf2)


(*
On met ça la car les dépendances ....
*)
type model = (BFO.atom*BFO.value) list












