
module M = Ast_modal
module L = List
module A = Array
module R = Random
module P = Pprinter

let file = Sys.argv.(1)

let max_var = 20

let tire_var () = 
    let _ = R.self_init () in
    R.int max_var

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
		match (R.int 4)  with 
		| 0 -> M.Conj (tire_form (n-1),tire_form (n-1))
		| 1 -> M.Dij (tire_form (n-1),tire_form (n-1))
		| 2 -> M.Boxe (tire_form(n-1))
		| _ -> M.Diamond (tire_form (n-1))
	end;

end

let get_prof () = 
	try int_of_string Sys.argv.(2)
	with _ -> 
		print_endline "le deuxième argument est la profondeur";
		exit 0



let _ = 
    let prof = get_prof () in
	let f = tire_form prof in	
	let bml = open_out (file^".InToHyLo") in
		output_string bml ("begin\n");
        output_string bml ((P.aux_m f)^"\n");
		output_string bml ("end\n");


