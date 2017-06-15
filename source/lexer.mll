{
	open Lexing
	open Parser

	exception Lex_err of string

	let newline lexbuf = 
		let pos = lexbuf.lex_curr_p in
		lexbuf.lex_curr_p <-
			{pos with pos_lnum = pos.pos_lnum +1;
			          pos_bol = pos.pos_cnum}



	
}

let chiffre = ['0'-'9']
let alpha = ['a'-'z']
let ident = alpha (alpha | chiffre | '_')*

rule next_token = parse
| ' ' | '\t' {next_token lexbuf}
| '\n' | '\r' {newline lexbuf; next_token lexbuf}
| ";" {comment lexbuf}
| "(" { LPAR}
| ")" { RPAR}
| "~" {Not}
| "&" {Conj}
| "|" {Dij}
| "=>" {Impl}
| "<>" {Diamond}
| "[]" {Boxe}
| ident as id {Ident id}
| _ as s { raise (Lex_err  ("illegal character: " ^ (String.make 1 s))) }

and comment = parse
| '\n' {newline lexbuf; next_token lexbuf}
| eof {EOF}
| _ {comment lexbuf}
