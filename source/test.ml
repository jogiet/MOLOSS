(*########################################################*)
(*          Fichier de tests divers & variés              *)
(*########################################################*)
module FO = Ast_fo
module M = Ast_modal
module C = Convertisseur
module PP = Pprinter

module R = Random
module L = List
module Sy = Sys
module S = String

module So = Solve


let pf = Printf.printf

(*--------------------------------------------------------*)
(*            Génération aléatoire de formule             *)
(*--------------------------------------------------------*)
let variables = ["p";"q";"p'";"q'"]

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
		match R.int 6 with 
		| 0 -> M.Atom (tire_var ())
		| 1 -> M.Not (M.Atom (tire_var ()))
		| 2 -> M.Conj (tire_form (n-1),tire_form (n-1))
		| 3 -> M.Dij (tire_form (n-1),tire_form (n-1))
		| 4 -> M.Boxe (tire_form (n-1))
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


let _ = 
	let nb,n = get_arg () 
	in
	for i = 1 to nb do
	let f = tire_form n in
	let f0 = C.st "w" f
	and a = tire_ax ()
	and out = open_out "test.out"
	in begin
		print_debug "\n \n =====================================\n ";
		print_debug "On commence une nouvelle formule \n";
		print_debug "Formule modale : \n";
		print_debug "Liste des axiomes : \n";
		L.iter (fun s -> pf "%s\n" s) a;
		flush_all ();
		PP.print_m f;
		So.solve f0 a (Some out);
	end;
	done
