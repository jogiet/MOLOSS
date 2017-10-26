# MOLOSS, a MOdal LOgic Solver via SMT [![Build Status](https://travis-ci.org/Meleagant/MOLOSS.svg?branch=master)](https://travis-ci.org/Meleagant/MOLOSS)

MOLOSS is a satisfiability solver for modal logics. It is an
implementation (and more) of C. Aceres, P. Fontaine and S. Merz paper
"[Modal Satisfiability via SMT solving](https://doi.org/10.1007/978-3-319-15545-6_5)" in which the authors use a
translation of propositional modal logic to first-order
logic.

## Copyright

   MOLOSS is licensed under the GNU General Public License v3. See
   the `COPYING` file for more details.

## Installation

   For the moment, you can only install MOLOSS manually. To install
   MOLOSS, you need:

   - an [OCaml](http://ocaml.org/) compiler (version >= 4.0.3)
   - the [Menhir](http://gallium.inria.fr/`fpottier/menhir/) parser generator
   - the OCaml library [mSat](https://github.com/Gbury/mSAT)
   - the OCaml library [ocaml-minisat](https://github.com/c-cube/ocaml-minisat) to interact with the MiniSat
     solver

   All these tools can be easily installed using the [OPAM](https://opam.ocaml.org/) OCaml
   Package Manager:

   #+BEGIN_SRC shell
   opam install menhir msat minisat
   #+END_SRC

   You can then build the system with

   #+BEGIN_SRC shell
   make
   #+END_SRC

   Notice also that:

   - (for the moment) MOLOSS uses UNIX system calls, so you must be on
     a UNIX/Linux system to use it
   - MOLOSS uses [Z3](https://github.com/Z3Prover/z3), a SMT solver.
	 You must have a Z3 installed
	 if you want to use it as an oracle or directly (see
     further).

## Usage

   The `moloss` executable takes as an argument a file containing the
   frame axioms of the logic and the formula to be checked the (see
   examples in the `data` directory). MOLOSS then output `SAT` or
   `UNSAT`. Be default, MiniSat is used as an oracle.

   Options:

   - `--all`: all SAT oracles are used (MiniSat, Z3, mSAT)
   - `--z3`, `--mSAT`: only the corresponding oracle is used
   - `--time`: print the total execution time (including parsing)
   - `--get-model`: print the assertions deduced by the solver and the
     Kripke model if the formula is satisfiable
   - `--direct`: use Z3 as a first-order solver on the translation of
     modal formulas into first-order formulas (no instanciation
     procedures)

   Not yet avaible options:

   - `--get-proof`: print a proof unsatisfiability if the formula is
     unsatisfiable (Z3 only)
   - `--soft`: use `assert-soft` constraints, even if not needed by
     the considered modal logic
   - `--soft-ignore`: do not use `assert-soft` constraints, even if
     needed (beware, infinite loop possible in this case!)

## Acknowledgments

    MOLOSS has been partly developed during an internship at
    [ISAE-SUPAERO](https://www.isae-supaero.fr/en/) and
    [ONERA](http://www.onera.fr/en).
