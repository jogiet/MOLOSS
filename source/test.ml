(*########################################################*)
(*          Fichier de tests divers & variés              *)
(*########################################################*)
module FO = Ast_fo
module M = Ast_modal
module C = Convertisseur
module PP = Pprinter
module U = Unix
module A = Array
module R = Random
module L = List
module Sy = Sys
module D = Direct
module S = String

module So = Solve


let pf = Printf.printf

(*--------------------------------------------------------*)
(*            Génération aléatoire de formule             *)
(*--------------------------------------------------------*)
let variables = ["p";"q";"p2";"q2"]

let tire_var () =
begin
	R.self_init ();
	L.nth variables (R.int 4);
end


let rec tire_form n = 
begin
	R.self_init ();
	match n with
	| 0 -> 
	begin
		match R.int 2 with
		| 0 -> M.Atom (tire_var () )
		| _ -> M.Not (M.Atom (tire_var () ))
	end
	| n -> 
	begin
		match (R.int 3) +2  with 
		| 8 -> M.Atom (tire_var ())
		| 9 -> M.Not (M.Atom (tire_var ()))
		| 2 -> M.Conj (tire_form (n-1),tire_form (n-1))
		| 3 -> M.Dij (tire_form (n-1),tire_form (n-1))
		| 4 -> M.Boxe (tire_form(n-1))
		| _ -> M.Diamond (tire_form (n-1))
	end;

end


(*--------------------------------------------------------*)
(*                    Pour les axiomes                    *) 
(*--------------------------------------------------------*)

let tire_ax () = 
	let ax_a = [|"-M";"-4";"-B";"-5";"-CD"|] in
	let res = ref [] 
	in begin
		for i = 0 to 4 do 
			if R.int 2 = 0 then
				res := ax_a.(i)::(!res);
		done;
		!res;
	end
		
let get_logic () = 
	let argv = A.to_list (Sy.argv) in
	if L.mem "-T" argv then
		["-M"]
	else if L.mem "-B" argv then
		["-B"]
	else if L.mem "-4" argv then
		["-4"]
	else if L.mem "-5" argv then
		["-5"]
	else if L.mem "-S4" argv then
		["-M";"-4"]
	else if L.mem "-S5" argv then
		["M";"-5"]
	else if L.mem "-K" argv then
		[]
	else
		tire_ax ()
	




(*--------------------------------------------------------*)
(*                Les tests en question                   *) 
(*--------------------------------------------------------*)

let handle = function
| C.MeauvaisFormat s ->
	pf "Erreur : meauvais format \n %s \n" s
| C.FreeVDM (v1,v2,s) ->
	pf "Erreur : les variables %s et %s ne matchent pas : \n %s \n" v1 v2 s
| _ -> ()

let print_debug s = 
begin
	print_string s;
	flush_all ();
end

let get_arg () = 
	let nb = 
		try int_of_string (Sy.argv.(1))
		with
		| _ ->
		begin
			pf "le premier argument est le nb d'essai \n";
			exit 1;
		end
	and n =
		try int_of_string (Sy.argv.(2))
		with
		| _ ->
		begin
			pf "le second argument est la profondeur max \n";
			exit 1;
		end
	in (nb,n)

let rec check_form = function
| M.Atom _ -> true
| M.Not f -> check_form f
| M.Conj (f1,f2) | M.Dij (f1,f2) | M.Impl (f1,f2) ->
	(check_form f1) && (check_form f2)
| M.Diamond f -> check_form f
| M.Boxe f -> match f with
	| M.Diamond _ -> false
	| _ -> check_form f

let _ = 
let nb,n = get_arg () 
and t0 = ref 0.
and t_mol = ref 0.
and t_z3 = ref 0.
and out = None (* Some (open_out "test.out") *)
in begin
	for i = 1 to nb do
		let f = tire_form n in
		let f0 = C.st "w" f
		and a = get_logic ()
		in begin
			pf "========================= \n";
			flush_all ();
	 		t0 := U.gettimeofday () ;
			So.solve f0 a out;
			t_mol := !t_mol +.(U.gettimeofday () -. !t0); 

	 		t0 := U.gettimeofday () ;
			D.solve f0 a out;
			t_z3 := !t_z3 +.(U.gettimeofday () -. !t0); 
		end;
	done;
	pf "Calculs effectués en : \n" ;
	pf "Pour Moloss : %f \n"
		((!t_mol)/. (float_of_int nb));
	pf "Pour z3 : %f \n"
		((!t_z3)/. (float_of_int nb));
	flush_all ();

end
