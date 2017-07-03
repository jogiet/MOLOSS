{
	open Lexing
	open Sp_parser

	exception Lex_err of string

	let newline lexbuf = 
		let pos = lexbuf.lex_curr_p in
		lexbuf.lex_curr_p <-
			{pos with pos_lnum = pos.pos_lnum +1;
			          pos_bol = pos.pos_cnum}

	let kwd_tbl = 
	[("true",TRUE);
	 ("false",FALSE);
 	 ("and",AND);
	 ("or",OR);
	 ("not",Not);
	 ("implies",Implies);
	 ("implied",Implied);
	 ("equiv",Equiv);
	 ("dia",Dia);
	 ("some",Dia);
	 ("box",Boxe);
	 ("all",Boxe);
	 ("begin_problem",BeginP);
	 ("end_problem",EndP);
	 ("list_of_descriptions",LoDesc);
	 ("list_of_symbols",LoSym);
	 ("list_of_special_formulae",LoSF);
	 ("list_of_settings",LoSet);
	 ("end_of_list",EoL);
	 ("EML",EML);
	 ("eml",EML);
	 ("axioms",Axioms);
	 ("conjectures",Conjectures);
	 ("formula",Form);
	 ("prop_formula",PForm);
	 ("concept_formula",CForm);
	 ("predicates",Predicates);
	 ("EMLTheory",THRY);
	 ("SPASS",SPASS);
	 ("set_flag",SetFlag)
	 ]

	let check_kwd s = 
		if List.mem_assoc s kwd_tbl then
			List.assoc s kwd_tbl			
		else
			Ident s

	
}


let digit = ['0'-'9']
let number = digit+
let letter = ['a'-'z' 'A'-'Z']
let spec_symb = '_'
let ident = (letter|digit|spec_symb)+

rule next_token = parse
| ' ' | '\t' {next_token lexbuf}
| '\n' | '\r' {newline lexbuf; next_token lexbuf}
| ";" {comment lexbuf}
| "." {DOT}
| "," {COMMA}
| "(" { LPAR}
| ")" { RPAR}
| "[" {LCRO}
| "]" {RCRO}
| "{" {LACC}
| "}" {RACC}
| "*" {TIMES}
| eof {EOF}
| number as n {Int (int_of_string n)}
| ident as id {check_kwd id}
| _ as s { raise (Lex_err  ("illegal character: " ^ (String.make 1 s))) }

and comment = parse
| '\n' {newline lexbuf; next_token lexbuf}
| eof {EOF}
| _ {comment lexbuf}
