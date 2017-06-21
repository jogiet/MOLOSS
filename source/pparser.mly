%{
open Ast_proof

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
%type <Ast_proof.file> s0

%%

s0:
LPAR; LPAR; PROOF; f = file; RPAR;RPAR ; EOF{f}

file:
| p = proof {Proof p}
| LPAR; LET; LPAR; LPAR ; s = ProofRef; p = proof; RPAR; RPAR ; f = file;RPAR
	{DeclP (s,p,f)} 
| LPAR; LET; LPAR; LPAR ; s = FormRef; f0 = formule; RPAR; RPAR ; f = file;RPAR
	{DeclF (s,f0,f)} 

proof:
| s = ProofRef {Refp s}
| LPAR; Axiom; f = formule; RPAR {Axiom f}
| LPAR; Asserted; f = formule; RPAR {Asserted f}
| LPAR; AndElim; p = proof; f = formule; RPAR {AndElim (p,f)}
| LPAR; MP; p1 = proof; p2 = proof; f = formule; RPAR {MP (p1,p2,f)}
| LPAR; Rewrite; LPAR ; Equal; f1 = formule ;f2 = formule;RPAR;RPAR {Rewrite (f1,f2)}
| LPAR; Unit; p =proof; pl = proof+; f = formule; RPAR {Unit (p,pl,f)}


formule:
| s = FormRef {Reff s}
| i = Ident {Atom i}
| TRUE {TRUE}
| FALSE {FALSE}
| LPAR ; Impl; f0 = formule; f1 = formule ; RPAR {Impl (f0,f1)}
| LPAR ; Equiv; f0 = formule; f1 = formule ; RPAR {Equiv (f0,f1)}
| LPAR ; Conj; f0 = formule; f1 = formule ; RPAR {Conj (f0,f1)}
| LPAR ; Dij; f0 = formule; f1 = formule ; RPAR {Dij (f0,f1)}
| LPAR ; Not; f0 = formule; RPAR {Not (f0)}


