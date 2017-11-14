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
	[("and",Conj);
	 ("or",Dij);
	 ("not",Not);
	 ("let",LET);
	 ("asserted",Asserted);
	 ("and-elim",AndElim);
	 ("unit-resolution",Unit);
	 ("def-axiom",Axiom);
	 ("hypothesis",Hyp);
	 ("lemma",Lemma);
	 ("mp",MP);
	 ("trans",Trans);
	 ("monotonicity",Monotonicity);
	 ("rewrite",Rewrite);
	 ("proof",PROOF);
	 ("true",TRUE);
	 ("false",FALSE)]

	let check_kwd s =
		if List.mem_assoc s kwd_tbl then
			List.assoc s kwd_tbl
		else
			Ident s


}


let chiffre = ['0'-'9']
let alpha = ['a'-'z']
let dol = '$'
let aro = '@'
let ident = alpha (alpha | chiffre | '-' | '_')*
let pident = aro ident
let fident = dol ident

rule next_token = parse
| ' ' | '\t' {next_token lexbuf}
| '\n' | '\r' {newline lexbuf; next_token lexbuf}
| ";" {comment lexbuf}
| "(" { LPAR}
| ")" { RPAR}
| "=" {Equal}
| "=>" {Impl}
| eof {EOF}
| pident as id {ProofRef id}
| fident as id {FormRef id}
| ident as id {check_kwd id}
| _ as s { raise (Lex_err  ("illegal character: " ^ (String.make 1 s))) }

and comment = parse
| '\n' {newline lexbuf; next_token lexbuf}
| eof {EOF}
| _ {comment lexbuf}
