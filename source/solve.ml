module Solve (SMT : Sign.Smt) = 
struct

open Format
open Ast_fo
open Decide

module PP = Pprinter

let fpf = printf



(*--------------------------------------------------------*)
(*              Fonctions pour les axiomes                *)
(*--------------------------------------------------------*)


let axiom_to_dec_proc axiom = 
	let rec aux = function 
	| [] -> []
	| "-M"::q | "-boxeM"::q ->
		if L.mem "-5" axiom then
			softreflexiv::(aux q)
		else 
			reflexiv::(aux q)
	| "-4"::q -> transitivity::(aux q)
	| "-B"::q -> symmetric::(aux q)
	| "-5"::q -> euclidean::(aux q)
	| "-CD"::q -> functionnal::(aux q)
	| t::q ->
	begin
		fpf "Axiome inconne : %s \n" t;
		exit 1;
	end
	in let res = aux axiom in
	if L.mem  "-CD" axiom  || L.mem "-boxeM" axiom then
		forall::res
	else if L.mem "-4" axiom || L.mem "-5" axiom then
		forall::softexist::res
	else
		forall::res@[exist]

let get_init_flag axioms = 
	if L.mem  "-M" axioms then
		[Reflexiv]
	else 
		[]

(*--------------------------------------------------------*)
(*               Fonction d'initialisation                *)
(*--------------------------------------------------------*)

let new_config () = 
	let config = 
	{cardw = 1;
	 w = ["w"];
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

let rec init (config : config) init_flag = function 
(* 
renvoie la liste des fonctions encadrées, et les nouvelles variables 
et enrichit la config au fur et à mesure ...
*)
| [] -> 
	if List.mem Reflexiv init_flag then
		(*
		let fb,new_var = abs config.env (FO.Relation ("w","w"))
		in [fb],new_var
		*)
		[],[]
	else
		[],[]
| f::q -> 
	let f_box,new_var = abs config.env f in
	let f_rest,new_var_rest = init config init_flag q in
		f_box::f_rest, new_var@new_var_rest


let print_soluce config m = 
let aux (k,b) = 
	let f = H.find config.env k in
	match f with
	| FO.Atom _ | FO.Relation _ | FO.Not _ ->
	begin
		match b with
		| true -> PP.print_fo f
		| false -> PP.print_fo (FO.Not f)
	end
	| FO.Exists _ | FO.Forall _ -> ()
	| _ -> assert false
in begin
	fpf "liste des mondes : \n";
	L.iter (fun k  -> fpf "%s \n" k) config.w;
	flush_all ();
	fpf "\nPropriétés à vérifier :\n";	
	List.iter aux m;
	flush_all ();
end

(*--------------------------------------------------------*)
(*                    Fonction Core                       *)
(*--------------------------------------------------------*)


let solve f a out = 
	let config = new_config () in
	let init_flag = get_init_flag a
	and dec_proc = axiom_to_dec_proc a in	
	let fo_box, new_var = init config init_flag [f] 
	and cont = ref true 
	in begin
		SMT.init ();		
		L.iter (fun v -> SMT.dec_const v ) new_var; 
		L.iter (fun fb -> SMT.dec_assert fb ) fo_box	;
		while !cont do
			match SMT.get_ans () with
			| SMT.UNSAT ->
			let p = () (*      get_proof oc ic out*)
			in begin
				fpf "\027[31mLa formule est insatisfiable \027[0m\n";
				(*
				PP.print_proof config.env p;
				*)
				p |> ignore;
				flush_all ();
				cont := false; 
			end
			| SMT.SAT  m ->
				try begin
					L.iter (fun d_proc -> d_proc config m) dec_proc;
					fpf "\027[92mLa formule est satisfiable \027[0m\n";
					(*
					print_soluce config m;
					*)
					flush_all ();
					cont := false; 
				end
				with
				| Found (new_var,new_bf) ->
				begin
					List.iter (fun v -> SMT.dec_const v) new_var;
					SMT.dec_assert new_bf ;
				end;
				| SoftFound (new_var1,new_bf,new_var2,bf_soft,wght) ->
				begin
					List.iter (fun v -> SMT.dec_const v ) new_var1;
					List.iter (fun v -> SMT.dec_const v ) new_var2;
					SMT.dec_assert new_bf;
					SMT.dec_assert_soft bf_soft wght;
				end
		done;
		SMT.close ();
	end
					
			


end
