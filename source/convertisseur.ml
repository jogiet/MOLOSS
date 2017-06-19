(*########################################################*)
(*             Convertisseur de Modal -> FO               *)
(*########################################################*)

module S = String

module M = Ast_modal
module FO = Ast_fo.FO
module PP = Pprinter

let x0 = "x"
and y0 = "y"

exception MeauvaisFormat of string
exception FreeVDM of string*string*string
(* When Free Variables don't match *)


(*--------------------------------------------------------*)
(*                       Modal -> FO                      *)
(*--------------------------------------------------------*)

let rec prop_neg = function
| M.Atom p -> M.Not (M.Atom p)
| M.Not f -> f (* Tertium non datur *)
| M.Conj (f1,f2) -> M.Dij (prop_neg f1,prop_neg f2)
| M.Dij (f1,f2) -> M.Conj (prop_neg f1,prop_neg f2)
| M.Impl (f1,f2) -> M.Conj (f1,prop_neg f2)
| M.Boxe f -> M.Diamond (prop_neg f)
| M.Diamond f -> M.Boxe (prop_neg f)


let rec st x = function 
| M.Atom p -> FO.Atom (S.uppercase p,x) 
| M.Not f ->  (st x (prop_neg f))
| M.Conj (f1,f2) -> FO.Conj (st x f1,st x f2)
| M.Dij (f1,f2) -> FO.Dij (st x f1,st x f2)
| M.Impl (f1,f2) -> st x (M.Conj (prop_neg f1,f2))
| M.Boxe f ->
		let y = if x = x0 then y0
						  else x0 in
		FO.Forall (y,FO.Dij
			(FO.Not (FO.Relation (x,y)),
			 st y f))
| M.Diamond f ->
		let y = if x = x0 then y0
						  else x0 in
		FO.Exists (y,FO.Conj
			(FO.Relation (x,y),
			 st y f))
					
let rec st_inv f0 =
match f0 with
| FO.Atom (p,x) -> (M.Atom (S.lowercase p),x)
| FO.Not f -> 
	let fm,x = st_inv f in 
	(M.Not fm,x)
| FO.Conj (f1,f2) -> 
	let fm1,x1 = st_inv f1
	and fm2,x2 = st_inv f2 in
	if x1 <> x2 then
		raise (FreeVDM (x1,x2,PP.aux_fo f0))
	else
		M.Conj (fm1,fm2),x1
| FO.Dij (f1,f2) -> 
	let fm1,x1 = st_inv f1
	and fm2,x2 = st_inv f2 in
	if x1 <> x2 then
		raise (FreeVDM (x1,x2,PP.aux_fo f0))
	else
		M.Dij (fm1,fm2),x1
| FO.Relation _ -> assert false 
(* le cas de la relation est traité dans celui des quantificateurs *)
| FO.Forall (i,f) -> 
begin
	match f with
	| FO.Dij (FO.Not (FO.Relation (x,y)),f2) ->
		let fm,y2 = st_inv f2 in
		if i <> y then
			raise (FreeVDM (i,y,PP.aux_fo f0))
		else if y2 <> y then
			raise (FreeVDM (y2,y,PP.aux_fo f0))
		else
			(M.Boxe fm,x)
	| _ -> raise (MeauvaisFormat (PP.aux_fo f0))
end
| FO.Exists (i,f) ->
begin
	match f with
	| FO.Conj (FO.Relation (x,y), f2) ->
		let fm,y2 = st_inv f2 in
		if i <> y then
			raise (FreeVDM (i,y,PP.aux_fo f0))
		else if y2 <> y then
			raise (FreeVDM (y2,y,PP.aux_fo f0))
		else
			(M.Diamond fm,x)
	| _ -> raise (MeauvaisFormat (PP.aux_fo f0))
end


		



































