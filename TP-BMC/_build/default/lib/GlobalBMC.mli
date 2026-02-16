(* $Id$ *)


(**
   Global Bounded Model Checking.

   This module solves the bounded model-checking problem for program automata
   (see {! Automaton}).  The implementation looks for a feasible path from the
   initial location to the final location.  It relies on the theorem prover Z3
   to check satisfiability of reachability formulas.

   @see <http://z3prover.github.io/api/html/ml/Z3.html> Z3
 *)

include BoundedModelChecking.S
