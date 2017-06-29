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
	 ("dia",Dia);
	 ("some",Dia);
	 ("boxe",Boxe);
	 ("begin-problem",BegP);
	 ("end-problem",EndP);
	 ("list_opf_symbols",LoSymb);
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
