
module M = Ast_modal
module L = List
module A = Array
module R = Random
module P = Pprinter

let file = Sys.argv.(1)

let get_logic () = 
	let argv = A.to_list (Sys.argv) in
	if L.mem "-T" argv then
		["-M"],"-T"
	else if L.mem "-B" argv then
		["-B"],"-B"
	else if L.mem "-4" argv then
		["-4"],"-4"
	else if L.mem "-5" argv then
		["-5"],"-5"
	else if L.mem "-CD" argv then
		["-CD"],"-CD"
	else if L.mem "-S4" argv then
		["-M";"-4"],"-S4"
	else if L.mem "-S5" argv then
		["-M";"-5"],"-S5"
	else if L.mem "-K" argv then
		[],"-K"
	else
		assert false

let variables = ["p";"q";"p2";"q2";"r";"r2";"s";"s2"]

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
	let ax,logic = get_logic () 
	and prof = get_prof () in
	let form = tire_form prof in	
	let out = open_out (file^".out") in
		output_string out (logic^","^(string_of_int prof)^"\n");
	let bml = open_out (file^".bml") in
		List.iter (fun s -> output_string bml (" "^s^" ")) ax;
		output_string bml " % \n";
		output_string bml ((P.aux_m form)^"\n")


