open Lexing
module PP = Pprinter
module P = Ast_proof

let fpf = Printf.printf

let report (b,e) file =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
fpf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc




let test filename =
let file = 
 try open_in filename
 with Not_found -> exit 0 in

	let lb = Lexing.from_channel file in 
try 
let a = Pparser.s0 Plexer.next_token lb in
	 P.extract a |> PP.print_proof
with 
			| Plexer.Lex_err s ->
			report (lexeme_start_p lb, lexeme_end_p lb) filename;
			fpf "lexical error: %s.\n" s;
			flush_all ();
			exit 1
  			| Pparser.Error ->
			report (lexeme_start_p lb, lexeme_end_p lb) filename;
			fpf "syntax error.\n";
			flush_all ();
			exit 1
			| _ ->
			report (lexeme_start_p lb, lexeme_end_p lb) filename;
			flush_all ();
			exit 1

let _ =
	List.iter test ["b";"c"]
















