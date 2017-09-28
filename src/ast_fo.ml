(*########################################################*)
(*         AST pour les formules du premier ordre         *)
(*########################################################*)



(*--------------------------------------------------------*)
(*                Formules du 1er ordre                   *)
(*--------------------------------------------------------*)


module Smap = Map.Make(String)
module Sset = Set.Make(String)

(** module for the first-order formula *)
module FO  = struct 

type ident = string

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

(** In order to use the Hashtbl module *)
type t = formula


(** In order to use the Hashtbl module *)
let compare = compare
(* Pour pouvoir donner une structure de Map *)

(*
les fonctions suivantes servent à gérer les variables libres et
notamment la question des variables quantifiées 
*)

(** The following functions and exceptions are used to handle free
variables and variables change *)


exception NotSameFV of (string*string)

(** return the free variable of a formula *)
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

(** return true iff the two formulas are the same modulo the free
variable *)
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
			

(** return a formula equal to the one given in argument but with the
given in argument free variable *)
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

(** module for the boxed first-order formula *)
module BFO = struct

(** This type for atom in boxed first order formulae *)
type atom = string

type formula = 
	| Atom of atom
	| Not of formula
	| Conj of formula *formula
	| Dij of formula *formula

(** In order to use *.Make functors *)
type t = formula 

(** In order to use *.Make functors *)
let compare = compare

	type value = bool

end


(*--------------------------------------------------------*)
(*                  Fonctions utiles                      *)
(*--------------------------------------------------------*)

let spf = Printf.sprintf
module H = Hashtbl

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

(** An abstraction function : Tansforms the given formula into a boxed 
formula and add the atom in
the envirronment by side effect *)
let rec abs env f = 
(*
transforme f en une formule encadrée et enrichit env si besoin
la fonction abs garantit que deux formules quantifiées égales modulo la
variable quantifiée deviennent égale dans abs et dans l'env
On ne doit donc plus s'en soucier ...
*)
match f with
| FO.Atom _ | FO.Exists _ | FO.Forall _ | FO.Relation _ -> 
	let found = ref None in
	let aux key f0 = 
		if (FO.equalmodvquant f f0) then
			found := Some key
	in begin
		H.iter aux env;
		match !found with
		| Some key ->(BFO.Atom key,[])
		| None -> let v = get_fv () 
		in begin
			H.add env v f;
			(BFO.Atom v,[v]);
		end
	end
| FO.Not f -> 
	let bf,new_var = abs env f in
		(BFO.Not bf,new_var)
| FO.Conj (f1,f2) ->
	let bf1,new_var1 = abs env f1 in
	let bf2,new_var2 = abs env f2 in
		(BFO.Conj (bf1,bf2),new_var1@new_var2)
| FO.Dij (f1,f2) ->
	let bf1,new_var1 = abs env f1 in
	let bf2,new_var2 = abs env f2 in
		(BFO.Dij (bf1,bf2),new_var1@new_var2)

(** A concretisation function : The inverse function of abs *)
let rec conc env bf = 
match bf with 
| BFO.Atom i -> 
	(H.find env i )
| BFO.Not bf ->
	FO.Not (conc env bf)
| BFO.Conj (bf1,bf2) ->
	FO.Conj (conc env bf1, conc env bf2)
| BFO.Dij (bf1,bf2) ->
	FO.Dij (conc env bf1, conc env bf2)


(*
On met ça la car les dépendances ....
*)
(** The type for the model returned by the oracle, here for the
dependancies *)
type model = (BFO.atom*BFO.value) list












