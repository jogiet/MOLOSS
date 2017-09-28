{
	open Lexing
	open Z3_parser

	exception Lex_err of string

	let newline lexbuf = 
		let pos = lexbuf.lex_curr_p in
		lexbuf.lex_curr_p <-
			{pos with pos_lnum = pos.pos_lnum +1;
			          pos_bol = pos.pos_cnum}


	let kwd_tbl = 
	["true", TRUE;
	 "false", FALSE;
	 "model",MODEL;
	 "define-fun", DEFFUN;
	 "Bool", BOOL]
	
	let check_kwd s = 
		try List.assoc s kwd_tbl
		with _ -> IDENT s
	
}

let chiffre = ['0'-'9']
let integer = ['1'-'9'] chiffre*
let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (alpha|integer|'_'|'-')*

rule next_token = parse
| ' ' | '\t' {next_token lexbuf}
| '\n' | '\r' {newline lexbuf; next_token lexbuf}
| ";" {comment lexbuf}
| "(" { LPAR}
| ")" { RPAR}
| ident as id {check_kwd id}
| eof {EOF}
| _ as s { raise (Lex_err  ("illegal character: " ^ (String.make 1 s))) }

and comment = parse
| '\n' {newline lexbuf; next_token lexbuf}
| eof {EOF}
| _ {comment lexbuf}

and error = parse
| eof {EOF}
| _ {error lexbuf}















