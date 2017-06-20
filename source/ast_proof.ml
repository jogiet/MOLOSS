
module H= Hashtbl



type formula = 
	| TRUE | FALSE
	| Atom of string
	| Reff of string (* e.g. : $x12 *)
	| Impl of formula*formula
	| Equiv of formula*formula
	| Not of formula
	| Conj of formula*formula
	| Dij of formula*formula

type proof = 
	| Refp of string (* e.g. : @x21 *)
	| Axiom of formula
	| Asserted of formula
	| AndElim of proof*formula
	| MP of proof*proof*formula
	| Rewrite of formula*formula
	| Unit of proof*(proof list)*formula

let rec con = function 
| Refp _ -> assert false
| Axiom f -> f
| Asserted f -> f
| AndElim (_,f) -> f
| MP (_,_,f) -> f
| Rewrite (f1,f2) -> Equiv (f1,f2) (* à vérifier *)
| Unit (_,_,f) -> f


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
















