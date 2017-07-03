%{

	module M = Ast_modal
	module L = List

%}

%token LPAR RPAR LACC RACC LCRO RCRO
%token DOT COMMA TIMES EOF
%token <string> Ident
%token <int> Int
%token BeginP EndP EoL
%token LoDesc LoSym LoSF LoSet
%token Form PForm CForm
%token EML
%token Predicates
%token Axioms Conjectures
%token SetFlag
%token THRY SPASS
%token TRUE FALSE
%token AND OR Implies Implied Equiv
%token Not Boxe Dia


%start problem
%type <Ast_modal.formula*(string list)> problem

%%

problem:
| BeginP ; LPAR ; Ident; RPAR; DOT; 
  description; 
  f = log_part ; 
  s = settings; 
  EndP; DOT; EOF
	{(f,
	  match s with
	  | Some x -> x
	  | None -> [])}

description:
| LoDesc; DOT ; pot_pourri* ; EoL; DOT {()}

log_part:
| symbol_list?;
  f = special_formula_list; 
   {f}
	

symbol_list: 
| LoSym ;DOT;  Predicates ; 
  LCRO;
  separated_nonempty_list(COMMA,symb_decl); 
  RCRO; DOT ; EoL ;DOT {()}

symb_decl:
| Ident {()}
| LPAR; Ident ; COMMA; Int; RPAR {()}


special_formula_list:
| LoSF; LPAR; origin_type; COMMA; EML;RPAR; DOT
  f = labelled_formula+;
  EoL; DOT
	{match f with
	 |  [] -> assert false
	 | t::q -> 
		let aux f1 f2 = M.Conj (f1,f2)
	 in L.fold_left aux t q}

origin_type:
| Axioms {()}
| Conjectures {()}

labelled_formula:
| prop_form_name; LPAR; f = prop_term ; c_label? ; RPAR; DOT {f}

prop_form_name:
| Form {()}
| PForm {()}
| CForm {()}

c_label:
| COMMA; Ident {()}

prop_term:
| TRUE {M.Dij (M.Atom "p",M.Not (M.Atom "p"))}
| FALSE {M.Conj (M.Atom "p",M.Not (M.Atom "p"))}
| OR ;LPAR; f1 = prop_term; COMMA; f2 = prop_term ; RPAR
	{M.Dij (f1,f2)}
| AND;LPAR; f1 = prop_term; COMMA; f2 = prop_term ; RPAR
	{M.Conj (f1,f2)}
| Not ; LPAR ; f = prop_term; RPAR
	{M.Not f}
| Implies; LPAR; f1 = prop_term; COMMA; f2 = prop_term; RPAR
	{M.Dij (M.Not f1,f2)}
| Implied; LPAR; f1 = prop_term; COMMA; f2 = prop_term; RPAR
	{M.Dij (M.Not f2,f1)}
| Equiv; LPAR; f1 = prop_term; COMMA; f2 = prop_term; RPAR
	{M.Conj (M.Dij (M.Not f2,f1),
		 M.Dij (M.Not f1,f2))}
| Boxe; LPAR; Ident; COMMA; f = prop_term ; RPAR
	{M.Boxe f}
| Dia; LPAR; Ident; COMMA; f = prop_term ; RPAR
	{M.Diamond f}
| i = Ident {M.Atom i}

settings:
| LoSet ; LPAR; SPASS; RPAR;DOT
  LACC; TIMES;
  a = flag+;
  TIMES; RACC;
  EoL; DOT; 
  	{let aux = List.filter
			(fun x -> match x with
			 | Some _ -> true
			 | _ -> false)
			a
	in
	match aux with
	| [x] -> x
	| [] -> None
	| _ -> assert false}

flag:
| SetFlag; LPAR; THRY; COMMA; n=Int; RPAR; DOT
	{Some (match n with
			| 2 -> ["-M"]
			| 3 -> ["-B"]
			| 4 -> ["-4"]
			| 5 -> ["-5"]
			| 6 -> ["-M";"-4"]
			| 7 -> ["-M";"-5"]
			| _ -> assert false)}
| SetFlag; pot_pourri_l*; DOT {None}

pot_pourri:
| Ident {()}
| Int {()}
| DOT {()}
| COMMA {()}
| LPAR {()}
| RPAR {()}
| LACC {()}
| RACC {()}
| TIMES {()}


pot_pourri_l:
| Ident {()}
| Int {()}
| COMMA {()}
| LPAR {()}
| RPAR {()}
| LACC {()}
| RACC {()}
| TIMES {()}









