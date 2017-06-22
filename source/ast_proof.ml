module H= Hashtbl

module All = struct

type all = 
	| TRUE | FALSE
	| Atom of string
	| Reff of string (* e.g. : $x12 *)
	| Refp of string (* e.g. : @x21 *)
	| Impl of all*all
	| Equiv of all*all
	| Not of all
	| Conj of all*all
	| Dij of all*all
	| Axiom of all
	| Asserted of all
	| AndElim of all*all
	| MP of all*all*all
	| Rewrite of all*all
	| Unit of all*(all list)
	| Monotonicity of all list
	| Equal of all*all

type file = 
	| Declf of string*all*file
	| Declp of string*all*file
	| Proof of all

end

module Proof = struct

type formula = 
	| TRUE | FALSE
	| Atom of string
	| Reff of string (* e.g. : $x12 *)
	| Impl of formula*formula
	| Equiv of formula*formula
	| Not of formula
	| Conj of formula*formula
	| Dij of formula*formula
	| Equal of formula*formula

type proof = 
	| Refp of string (* e.g. : @x21 *)
	| Axiom of formula
	| Asserted of formula
	| AndElim of proof*formula
	| MP of proof*proof*formula
	| Rewrite of formula*formula
	| Unit of proof*(proof list)*formula
	| Monotonicity of (proof list)*formula

let rec con = function 
| Refp _ -> assert false
| Axiom f -> f
| Asserted f -> f
| AndElim (_,f) -> f
| MP (_,_,f) -> f
| Rewrite (f1,f2) -> Equiv (f1,f2) (* à vérifier *)
| Unit (_,_,f) -> f
| Monotonicity (_,f) -> f


type penv = (string,proof) H.t
type fenv = (string,formula) H.t

let rec inlinef fenv = function
	| TRUE -> TRUE
	| FALSE -> FALSE
	| Atom a -> Atom a
	| Reff s -> H.find fenv s
	| Impl (f1,f2) -> Impl (inlinef fenv f1,inlinef fenv f2)  
	| Equiv (f1,f2) -> Equiv (inlinef fenv f1,inlinef fenv f2)  
	| Conj (f1,f2) -> Conj (inlinef fenv f1,inlinef fenv f2)  
	| Dij (f1,f2) -> Dij (inlinef fenv f1,inlinef fenv f2)  
	| Not f -> Not (inlinef fenv f)
	| Equal (f1,f2) -> Equal (inlinef fenv f1,inlinef fenv f2)  

let rec inlinep penv fenv = function 
	| Refp s -> H.find penv s
	| Axiom f -> Axiom (inlinef fenv f)
	| Asserted f -> Asserted (inlinef fenv f)
	| AndElim (p,f) -> AndElim (inlinep penv fenv p,inlinef fenv f)
	| MP (p1,p2,f) -> MP (inlinep penv fenv p1,inlinep penv fenv p2,
			inlinef fenv f)
	| Rewrite (f1,f2) -> Rewrite (inlinef fenv f1,inlinef fenv f2)
	| Unit(p,pl,f) -> Unit (inlinep penv fenv p,
			List.map (fun p -> inlinep penv fenv p) pl,
			inlinef fenv f)
	| Monotonicity (pl,f) ->
			Monotonicity (List.map (fun p -> inlinep penv fenv p) pl,
						  inlinef fenv f)

type file = 
	| DeclF of string*formula*file
	| DeclP of string*proof*file
	| Proof of proof

let extract p = 
	(* Renvoie une preuve sans les varaibles $x.. et @x.. *)
	let rec aux penv fenv = function
	| Proof p -> inlinep penv fenv p
	| DeclF (s,f,p) -> 
		let f_tot = inlinef fenv f 
		in begin
			H.add fenv s f_tot;
			aux penv fenv p;
		end
	| DeclP (s,p0,p) -> 
		let p_tot = inlinep penv fenv p0 
		in begin
			H.add penv s p_tot;
			aux penv fenv p;
		end
	in aux (H.create 42) (H.create 42) p

end

module A = All
module P = Proof

(*   Fonctions de conversion All -> Proof    *)

let rec conv_form = function
| A.TRUE -> P.TRUE
| A.FALSE -> P.FALSE
| A.Reff s -> P.Reff s
| A.Atom s -> P.Atom s
| A.Not f -> P.Not (conv_form f)
| A.Impl (f1,f2) -> P.Impl (conv_form f1,conv_form f2) 
| A.Equiv (f1,f2) -> P.Equiv (conv_form f1,conv_form f2) 
| A.Conj (f1,f2) -> P.Conj (conv_form f1,conv_form f2) 
| A.Dij (f1,f2) -> P.Dij (conv_form f1,conv_form f2) 
| _ -> assert false

and aux_unit = function
| [] -> assert false
| [x] -> [],(conv_form x)
| t::q -> 
	let pl,f = aux_unit q in
	(conv_proof t)::pl,f


and conv_proof = function
| A.Axiom f -> P.Axiom (conv_form f)
| A.Refp s -> P.Refp s
| A.Asserted f -> P.Asserted (conv_form f)
| A.AndElim (p,f) -> P.AndElim (conv_proof p,conv_form f)
| A.MP (p1,p2,f) ->P.MP (conv_proof p1,conv_proof p2,conv_form f)
| A.Rewrite (f1,f2) -> P.Rewrite (conv_form f1,conv_form f2)
| A.Unit (p,pl) -> 
	let p = conv_proof p
	and pl,f = aux_unit pl in
		(P.Unit (p,pl,f))
| A.Monotonicity pl -> 
	let pl,f = aux_unit pl in
	P.Monotonicity (pl,f)
| _ -> assert false

and conv_file = function
| A.Declf (s,f,fl) -> P.DeclF (s,conv_form f,conv_file fl)
| A.Declp (s,p,fl) -> P.DeclP (s,conv_proof p,conv_file fl)
| A.Proof p -> P.Proof (conv_proof p)




let traite file = conv_file file |> P.extract






