%{

	open Ast_modal

%}

%token ...

%start problem
%type <Ast_modal.formula> problem

%%

problem:
| BeginP ; LPAR ; Ident; RPAR; DOT; description; f = log_part ; s = settings; 
  EndP
	{(f,s)}

description:
| LoDesc ; ; EoL

log_part:
| symbol_list?; 
  declarartion_list? ; 
  formula_list*; 
  f = special_formula_list; 
  clause_list*;
  proof_list*
	

| symbol_list: 
LoSym ; Predicates ; 
	LCRO ;
	separated_nonempty_list(COMMA,symb_decl);
	
	RCRO; 
EoL {()}

symb_decl = 
	| ident {()}
	| LPAR; ident ; COMMA; arity; RPAR {()}


special_formula_list:
LoSF; LPAR; origin_type; COMMA; EML;RPAR; DOT
f = labelled_formula+;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        EoL; DOT
	{match f with
	 |  [] -> assert false
	 | t::q -> 
		let aux f1 f2 -> M.Conj (f1,f2)
	 in L.fold_left aux t q}

labelled_formula:
|prop_form_name; LPAR; f = prop_term ; c_label?; RPAR {f}

c_label:
| COMMA; Ident {()}

prop_term:
| TRUE
| FALSE
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
| Box; LPAR; Ident; COMMA; f = prop_term ; RPAR
	{M.Boxe f}
| Dia; LPAR; Ident; COMMA; f = prop_term ; RPAR
	{M.Diamond f}
| i = Ident {M.Atom i}















