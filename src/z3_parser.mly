

%{

%}
/* Declaration des tokens */

%token LPAR RPAR
%token MODEL
%token EOF
%token BOOL DEFFUN TRUE FALSE
%token <string> IDENT

%start answer
%type <(Ast_fo.BFO.atom*bool) list> answer

%%

answer :
|  m = model ; EOF
	{ m}

model :
| LPAR;MODEL;m = affect*;RPAR {m}

affect :
| LPAR;DEFFUN; i = IDENT;LPAR;RPAR;BOOL;c = getbool; RPAR
	{(String.sub i 1 (String.length i -1) |> int_of_string, c)}

getbool :
| TRUE {true}
| FALSE {false}
