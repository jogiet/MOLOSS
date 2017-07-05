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
| f1 = atom; Conj; f2 = andrule {Ast_modal.Conj (f1,f2)}
| f1 = atom; Dij; f2 = orrule {Ast_modal.Dij (f1,f2)}
| f1 = atom; Impl; f2 = implrule {Ast_modal.Impl (f1,f2)}
| Boxe; f = boxrule {Ast_modal.Boxe f}
| Diamond; f = diarule {Ast_modal.Diamond f}

andrule:
| f1 = atom; Conj; f2 = andrule {Ast_modal.Conj (f1,f2)}
| f = atom {f}

orrule:
| f1 = atom; Dij; f2 = orrule {Ast_modal.Dij (f1,f2)}
| f = atom {f}

implrule:
| f1 = atom; Impl; f2 = implrule {Ast_modal.Impl (f1,f2)}
| f = atom {f}

boxrule:
| Boxe; f = boxrule {Ast_modal.Boxe f}
| f = atom {f}

diarule:
| Diamond; f = diarule {Ast_modal.Diamond f}
| f = atom {f}

atom:
| LPAR; f = formula; RPAR {f}
| Not; i = ident; {Ast_modal.Not (Ast_modal.Atom i)}
| Not; f = formula; {Ast_modal.Not f}
| i = ident; {Ast_modal.Atom i}

ident:
| i = Ident {i}

axiom:
| a = Axiom {a}
