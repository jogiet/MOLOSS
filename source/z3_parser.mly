

%{

%}
/* Declaration des tokens */

%token LPAR RPAR
%token MODEL
%token EOF
%token BOOL DEFFUN TRUE FALSE
%token <string> IDENT

%start answer
%type <(string*bool) list> answer

%%

answer :
|  m = model ; 
	{ m}

model :
| LPAR;MODEL;m = affect*;RPAR {m}

affect :
| LPAR;DEFFUN; i = IDENT;LPAR;RPAR;BOOL;c = getbool; RPAR
	{(i,c)}

getbool : 
| TRUE {true}
| FALSE {false}
