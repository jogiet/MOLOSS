(*###################################################################*)
(*                  Interface pour alt-ergo zero                     *)
(*###################################################################*)

(**
   This Module is an attempt to use the alt-Ergo-Zero solver.
   @deprecated Does not work yet.
*)

module A = Aez
module S = A.Smt
module Solver = S.Make ()
module H = Hashtbl


module SMTaez : Sign.Smt =
struct

let stoa = H.create 42

type ans =
	| UNSAT
	| SAT of (string*bool) list

let init () =  Solver.clear ()
let close () =  Solver.clear ()

let dec_const s = ()

let dec_assert f = ()

let dec_assert_soft f w = ()

let get_ans () = UNSAT

end
