%{
open Ast_proof.A

%}


%token LPAR RPAR
%token EOF

%token PROOF LET
%token Axiom Asserted
%token AndElim MP
%token Rewrite Unit
%token Equal

%token TRUE FALSE
%token Conj Dij Impl Equiv
%token Not 
%right LPAR

%token <string> Ident
%token <string> ProofRef
%token <string> FormRef


%start s0
%type <Ast_proof.A.file> s0


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
| s = ProofRef {Refp s}
| LPAR; Axiom; f = all; RPAR {Axiom f}
| LPAR; Asserted; f = all; RPAR {Asserted f}
| LPAR; AndElim; p = all; f = all; RPAR {AndElim (p,f)}
| LPAR; MP; p1 = all; p2 = all; f = all; RPAR {MP (p1,p2,f)}
| LPAR; Rewrite; LPAR ; Equal; f1 = all ;f2 = all;RPAR;RPAR {Rewrite (f1,f2)}
| LPAR; Unit; p =all; pl = all+; RPAR {Unit (p,pl)}
| s = FormRef {Reff s}
| i = Ident {Atom i}
| TRUE {TRUE}
| FALSE {FALSE}
| LPAR ; Impl; f0 = all; f1 = all ; RPAR {Impl (f0,f1)}
| LPAR ; Equiv; f0 = all; f1 = all ; RPAR {Equiv (f0,f1)}
| LPAR ; Conj; f0 = all; f1 = all ; RPAR {Conj (f0,f1)}
| LPAR ; Dij; f0 = all; f1 = all ; RPAR {Dij (f0,f1)}
| LPAR ; Not; f0 = all; RPAR {Not (f0)}


