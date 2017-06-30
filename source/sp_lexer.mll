{
	open Lexing
	open Pparser

	exception Lex_err of string

	let newline lexbuf = 
		let pos = lexbuf.lex_curr_p in
		lexbuf.lex_curr_p <-
			{pos with pos_lnum = pos.pos_lnum +1;
			          pos_bol = pos.pos_cnum}

	let kwd_tbl = 
	[("true",TRUE);
	 ("false",FALSE);
 	 ("and",Conj);
	 ("or",Dij);
	 ("not",Not);
	 ("implies",Implies);
	 ("implied",Implied);
	 ("equiv",Equiv);
	 ("dia",Dia);
	 ("some",Dia);
	 ("boxe",Boxe);
	 ("begin-problem",BegP);
	 ("end-problem",EndP);
	 ("list_of_symbols",LoSymb);
	 ("end_of_list",EoList);
	 ]

	let check_kwd s = 
		if List.mem_assoc s kwd_tbl then
			List.assoc s kwd_tbl			
		else
			Ident s

	
}


let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let spec_symb = '_'
let ident = (letter|digit|spec_symb)+

ule next_token = parse
| ' ' | '\t' {next_token lexbuf}
| '\n' | '\r' {newline lexbuf; next_token lexbuf}
| ";" {comment lexbuf}
| "(" { LPAR}
| ")" { RPAR}
| "{" {LACC}
| "}" {RACC}
| eof {EOF}
| pident as id {ProofRef id}
| fident as id {FormRef id}
| ident as id {check_kwd id}
| _ as s { raise (Lex_err  ("illegal character: " ^ (String.make 1 s))) }

and comment = parse
| '\n' {newline lexbuf; next_token lexbuf}
| eof {EOF}
| _ {comment lexbuf}
