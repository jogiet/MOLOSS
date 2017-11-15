%{
open Ast_modal

%}

%token LPAR RPAR
%token Diamond Boxe
%token TRUE FALSE
%token EOF
%token Conj
%token Dij
%token Impl
%token Equiv
%token Not
%token BEGIN END
%token <int> Prop

%right Conj
%right Dij
%right Impl
%right Equiv
%nonassoc Boxe Diamond Not

%start file
%type <(string list)*Ast_modal.formula> file

%%

file :
| BEGIN; f = formula ;END; EOF {[],f}

formula:
| TRUE {Ast_modal.Dij (Ast_modal.Atom 1,Ast_modal.Not (Ast_modal.Atom 1))}
| FALSE {Ast_modal.Conj (Ast_modal.Atom 1,Ast_modal.Not (Ast_modal.Atom 1))}
| f = atom {f}
| Not; f = formula {f}
| f1 = formula; Conj; f2 = formula {Ast_modal.Conj (f1,f2)}
| f1 = formula; Dij; f2 = formula {Ast_modal.Dij (f1,f2)}
| f1 = formula; Impl; f2 = formula {Ast_modal.Impl (f1,f2)}
| f1 = formula; Equiv; f2 = formula
    {Ast_modal.Conj (Ast_modal.Impl (f1,f2),
                     Ast_modal.Impl (f2,f1))}
| Boxe; f = formula {Ast_modal.Boxe f}
| Diamond; f = formula {Ast_modal.Diamond f}

atom:
| LPAR; f = formula; RPAR {f}
| i = Prop; {Ast_modal.Atom i}
