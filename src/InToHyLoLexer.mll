{
	open Lexing
	open InToHyLoParser

	exception Lex_err of string

	let newline lexbuf =
		let pos = lexbuf.lex_curr_p in
		lexbuf.lex_curr_p <-
			{pos with pos_lnum = pos.pos_lnum +1;
			          pos_bol = pos.pos_cnum}

}

let chiffre = ['0'-'9']
let ident = chiffre+
let proposition  = 'p' ident
let relation = 'r' ident
let boxe = '[' relation ']'
let diamond = '<' relation '>'

rule next_token = parse
| ' ' | '\t' {next_token lexbuf}
| '\n' | '\r' {newline lexbuf; next_token lexbuf}
| diamond {Diamond}
| boxe {Boxe}
| "(" { LPAR}
| ")" { RPAR}
| "~" {Not}
| "&" {Conj}
| "|" {Dij}
| "<->" {Equiv}
| "->" {Impl}
| "true" {TRUE}
| "false" {FALSE}
| "begin" {BEGIN}
| "end" {END}
| eof {EOF}
| proposition as p {Prop (int_of_string (String.sub p 1 (String.length p -1)))}
| _ as s { raise (Lex_err  ("illegal character: " ^ (String.make 1 s))) }
