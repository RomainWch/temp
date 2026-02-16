(* $Id$ *)


(**
   Bounded Model Checking.

   The bounded model-checking problem for program automata (see {! Automaton})
   asks whether there exists a feasible path {i of bounded length} from the
   initial location to the final location.  The theorem prover Z3 is used to
   check feasibility of paths.

   {L {b Implemented by:} {! DepthFirstBMC}, {! GlobalBMC}.}

   @see <http://z3prover.github.io/api/html/ml/Z3.html> Z3
 *)


(**
   Common interface to all bounded model-checking implementations.
 *)
module type S =
sig
  (**
     Type of possible results of the [fwd_search] and [bwd_search] functions.
   *)
  type result =
    | Path of (Command.t * Automaton.Node.t) list 
    (**
       Feasible path from the initial location to the final location.  The initial
       location is omitted from the path.  Formally, the path is given as a list
       {i (cmd{_ 1}, loc{_ 1}), ..., (cmd{_ k}, loc{_ k})} which stands for the
       path {i init, cmd{_ 1}, loc{_ 1}, ..., cmd{_ k}, loc{_ k}}.
     *)
    | Empty of bool
    (**
       No feasible path was found.  The boolean indicates the exhaustivity of the
       result.  If the boolean is [true], then the search was exhaustive, meaning
       that the program automaton under analysis contains no run from its initial
       location to its final location.  If the boolean is [false], then the search
       was not exhaustive.
     *)

  (**
     [fwd_search automaton bound verbose] solves the bounded model-checking
     problem for the input program [automaton] and the input [bound].

     - If it returns [Path p] then [p] is a feasible path of [automaton] of length
       at most [bound].
     - If it returns [Empty true] then [automaton] contains no feasible path.
     - If it returns [Empty false] then [automaton] contains no {i provably}
       feasible path of length at most [bound].

     {L {b Requires:} [bound] is a nonnegative integer.}
   *)
  val fwd_search : Automaton.t -> Z3.context -> int -> bool -> result

  (**
     [bwd_search automaton bound verbose] is identical to [fwd_search automaton
     bound verbose] except that the search is performed backwards from the final
     location.
   *)
  val bwd_search : Automaton.t -> Z3.context -> int -> bool -> result
end
