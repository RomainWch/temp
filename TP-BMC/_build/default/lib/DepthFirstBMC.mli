(* $Id$ *)


(**
   Depth-First Bounded Model Checking.

   This module solves the bounded model-checking problem for program automata
   (see {! Automaton}).  The implementation looks for a feasible path from the
   initial location to the final location.  It proceeds through a depth-first
   exploration of the program automaton (viewed as a graph).  The theorem prover
   Z3 is used to check feasibility of paths.

   @see <http://z3prover.github.io/api/html/ml/Z3.html> Z3
 *)

include BoundedModelChecking.S
