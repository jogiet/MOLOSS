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

   ```shell
   opam install menhir msat minisat
   eval $(opam config env)
   ```

   You can then build the system with

   ```shell
   make
   ```

   The previous command will build a native and byte code
   executable. You may also use more specific targets:

   - `profile` for native code executable with profiling
   - `debug` for byte code executable with debug
   - `static` for static native code executable with no dynamic
     libraries (even libc)

   Notice also that:

   - (for the moment) MOLOSS uses UNIX system calls, so you must be on
     a UNIX/Linux system to use it
   - MOLOSS uses [Z3](https://github.com/Z3Prover/z3), a SMT solver.
	 You must have a Z3 installed
	 if you want to use it as an oracle or directly (see
     further).

## Usage

   The `moloss` executable takes as an argument a file containing the
   formula to be checked in InToHyLo format (see examples in the `data` directory or
   [4.1](http://cs.ru.nl/paar16/paper-07.pdf)).
   MOLOSS then output `SAT` or `UNSAT`.
   By default, MiniSat is used as an oracle.

   Options:

   - `--all`: all SAT oracles are used (MiniSat, Z3, mSAT)
   - `--z3`, `--mSAT`: only the corresponding oracle is used
   - `--time`: print the total execution time (including parsing)
   - `--get-model`: print the Kripke model if the formula is satisfiable
   in the Flat Kripke Model (FKM) format.
   - `--get-assert`: print the assertionsmade by the solver and the
   final model if the formule is satisfiable.
   - `--direct`: use Z3 as a first-order solver on the translation of
   modal formulas into first-order formulas (no instanciation
   procedures)
   - `--soft`: use `assert-soft` constraints, even if not needed by
   the considered modal logic
   - `--soft-ignore`: do not use `assert-soft` constraints, even if
   needed (beware, infinite loop possible in this case!)

   Not yet avaible options:

   - `--get-proof`: print a proof unsatisfiability if the formula is
   unsatisfiable (Z3 only)

## Acknowledgments

   MOLOSS has been partly developed during an internship at
   [ISAE-SUPAERO](https://www.isae-supaero.fr/en/) and
   [ONERA](http://www.onera.fr/en).
