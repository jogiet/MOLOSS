%{
open Ast_modal

%}

/* Declaration des tokens */

%token LPAR RPAR
%token EOF
%token Conj Dij Impl
%token Boxe Diamond
%token Not Begin
%token <string> Ident
%token <string> Axiom

%start file
%type <(string list)*Ast_modal.formula> file

%%

file :
| a = axiom*; Begin; f = formula ; EOF {a,f}

formula:
| f = atom {f}
| f1 = atom; Conj; f2 = atom {Ast_modal.Conj (f1,f2)}
| f1 = atom; Dij; f2 = atom {Ast_modal.Dij (f1,f2)}
| f1 = atom; Impl; f2 = atom {Ast_modal.Impl (f1,f2)}
| Boxe; f = atom {Ast_modal.Boxe f}
| Diamond; f = atom {Ast_modal.Diamond f}

atom:
| LPAR; f = formula; RPAR {f}
| Not; i = ident; {Ast_modal.Not (Ast_modal.Atom i)}
| i = ident; {Ast_modal.Atom i}

ident:
| i = Ident {i}

axiom:
| a = Axiom {a}
