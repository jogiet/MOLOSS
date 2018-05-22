module GetSimplify (A : Sign.DecideArg) : Sign.Simplify = struct

  (** This module should make simplification on the formula *)

module M = Ast_modal
module FO = Ast_fo.FO
module BFO = Ast_fo.BFO
open Ast_fo

module L = List

let fpf = Printf.printf

let rec simplifyAux f =
  (** Makes simplification such as
      - ([] f) /\ ([] f') -> [] (f /\ f')
      - (<> f) \/ (<> f') -> <> (f \/ f') *)
  match f with
  | M.Conj (M.Boxe f1, M.Boxe f2 ) -> M.Boxe (simplifyAux (M.Conj (f1, f2)))
  | M.Dij (M.Diamond f1, M.Diamond f2 ) -> M.Diamond (simplifyAux (M.Dij (f1, f2)))
  | _ -> f

    (** Returns true iff f1 = ~f2 *)
let rec isNegation f1 f2 =
  match f1,f2 with
  | M.True, M.False | M.False, M.True -> true
  | M.Atom x1, M.Not (M.Atom x2) | M.Not (M.Atom x1), M.Atom x2 -> x1 = x2
  | M.Not f1aux, M.Not f2aux  -> isNegation f1aux f2aux
  | M.Conj (f1g,f1d), M.Conj (f2g,f2d)
  | M.Dij (f1g,f1d), M.Dij (f2g,f2d) ->
    ((isNegation f1g f2g) && (isNegation f1d f2d))
    ||
    ((isNegation f1g f2d) && (isNegation f1d f2g))
  | M.Boxe f1aux, M.Diamond f2aux | M.Diamond f1aux, M.Boxe f2aux -> isNegation f1aux f2aux
  | M.Impl (f1g,f1d), M.Impl (f2g,f2d) -> assert false
  | _ -> false

let rec simplifyK = function
  | M.True -> M.True
  | M.False -> M.False
  | M.Atom x -> M.Atom x
  | M.Not M.True -> M.False
  | M.Not M.False -> M.True
  | M.Not (M.Atom x) -> M.Not (M.Atom x)
  | M.Not f -> M.Not(simplifyK f)
  | M.Conj (f1, f2) ->
    begin
      match simplifyK f1, simplifyK f2 with
      | f1aux, f2aux when isNegation f1aux f2aux -> M.False
      | M.Boxe f1aux, M.Boxe f2aux -> M.Boxe (simplifyAux (M.Conj (f1, f2)))
      | _, M.False | M.False, _ -> M.False
      | f1aux, M.True -> f1aux
      | M.True, f2aux -> f2aux
      | _ -> M.Conj (f1,f2)
    end
  | M.Dij (f1, f2) ->
    begin
      match simplifyK f1, simplifyK f2 with
      | f1aux, f2aux when isNegation f1aux f2aux -> M.True
      | M.Diamond f1aux, M.Diamond f2aux -> M.Diamond (simplifyAux (M.Dij (f1aux,f2aux)))
      | _, M.True | M.True, _ -> M.True
      | f1aux, M.False -> f1aux
      | M.False, f2aux -> f2aux
      | f1aux, f2aux -> M.Dij (f1aux, f2aux)
    end
  | M.Impl (f1, f2) -> simplifyK (M.Dij (M.Not f1, f2))
  | M.Boxe f ->
    begin
      match simplifyK f with
      | M.True -> M.True
      | M.False -> M.Boxe M.False
      | faux -> M.Boxe faux
    end
  | M.Diamond f ->
    begin
      match  simplifyK f with
      | M.True -> M.Diamond M.True
      | M.False -> M.False
      | faux -> M.Diamond faux
    end

let rec simplifyS = function
  | M.True -> M.True
  | M.False -> M.False
  | M.Atom x -> M.Atom x
  | M.Not M.True -> M.False
  | M.Not M.False -> M.True
  | M.Not (M.Atom x) -> M.Not (M.Atom x)
  | M.Not f -> M.Not (simplifyS f)
  | M.Conj (f1, f2) ->
    begin
      match simplifyS f1, simplifyS f2 with
      | f1aux, f2aux when isNegation f1aux f2aux -> M.False
      | M.Boxe f1aux, M.Boxe f2aux -> M.Boxe (simplifyAux (M.Conj (f1, f2)))
      | _, M.False | M.False, _ -> M.False
      | f1aux, M.True -> f1aux
      | M.True, f2aux -> f2aux
      | _ -> M.Conj (f1,f2)
    end
  | M.Dij (f1, f2) ->
    begin
      match simplifyS f1, simplifyS f2 with
      | f1aux, f2aux when isNegation f1aux f2aux -> M.True
      | M.Diamond f1aux, M.Diamond f2aux -> M.Diamond (simplifyAux (M.Dij (f1aux,f2aux)))
      | _, M.True | M.True, _ -> M.True
      | f1aux, M.False -> f1aux
      | M.False, f2aux -> f2aux
      | f1aux, f2aux -> M.Dij (f1aux, f2aux)
    end
  | M.Impl (f1, f2) -> simplifyS (M.Dij (M.Not f1, f2))
  | M.Boxe f ->
    begin
      match simplifyS f with
      | M.True -> M.True
      | M.False -> M.False
      | faux -> M.Boxe faux
    end
  | M.Diamond f ->
    begin
      match  simplifyS f with
      | M.True -> M.True
      | M.False -> M.False
      | faux -> M.Diamond faux
    end

let getSimplify arg =
  if L.mem "--no-simplify" arg then
    (fun x -> x)
  else if L.mem "-S" arg  || L.mem "--S4" arg || L.mem "--S5" arg
    then simplifyS
    else simplifyK

let simplify = getSimplify A.argument


end
