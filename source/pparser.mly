%{

	open Ast_proof.All

%}


%token LPAR RPAR
%token EOF

%token PROOF LET
%token Axiom Asserted
%token AndElim MP
%token Rewrite Unit
%token Equal Monotonicity Trans

%token TRUE FALSE
%token Conj Dij Impl Equiv
%token Not 

%token <string> Ident
%token <string> ProofRef
%token <string> FormRef


%start s0
%type <Ast_proof.All.file> s0


%%

s0:
LPAR; LPAR; PROOF; f = file; RPAR;RPAR ; EOF{f}

file:
| p = all {Proof p}
| LPAR; LET; LPAR; LPAR ; s = ProofRef; p = all; RPAR; RPAR ; f = file;RPAR
	{Declp (s,p,f)} 
| LPAR; LET; LPAR; LPAR ; s = FormRef; f0 = all; RPAR; RPAR ; f = file;RPAR
	{Declf (s,f0,f)} 

all:
| i = Ident {Atom i}
| s = FormRef {Reff s}
| TRUE {TRUE}
| FALSE {FALSE}

| s = ProofRef {Refp s}
| LPAR; Axiom; f = all; RPAR {Axiom f}
| LPAR; Asserted; f = all; RPAR {Asserted f}
| LPAR; AndElim; p = all; f = all; RPAR {AndElim (p,f)}
| LPAR; MP; p1 = all; p2 = all; f = all; RPAR {MP (p1,p2,f)}
| LPAR; Rewrite; LPAR ; Equal; f1 = all ;f2 = all;RPAR;RPAR 
	{Rewrite (f1,f2)}
| LPAR; Monotonicity ;pl =  all+;RPAR {Monotonicity pl}
| LPAR; Unit; p =all; pl = all+; RPAR {Unit (p,pl)}
| LPAR; Trans; p = all;q = all; r = all ; RPAR ; {Trans (p,q,r)}

| LPAR ; Impl; f0 = all; f1 = all ; RPAR {Impl (f0,f1)}
| LPAR ; Equiv; f0 = all; f1 = all ; RPAR {Equiv (f0,f1)}
| LPAR ; Conj; f0 = all; f1 = all+ ; RPAR 
	{List.fold_left (fun f0 f1 -> Conj(f0,f1)) f0 f1}
| LPAR ; Dij; f0 = all; f1 = all+ ; RPAR 
	{List.fold_left (fun f0 f1 -> Dij(f0,f1)) f0 f1}
| LPAR ; Not; f0 = all; RPAR {Not (f0)}
| LPAR ; Equal ; f0 = all; f1 = all;RPAR {Equal (f0,f1)}


