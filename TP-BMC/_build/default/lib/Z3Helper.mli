(* $Id$ *)


(**
 * Helper module for Z3 formulas.
 *
 * @see <http://z3prover.github.io/api/html/ml/Z3.html>
 *   The Z3 ML/OCaml Interface
 *)


(**
 * The printable type of Z3 formulas.
 *)
 include Print.S with type t = Z3.Expr.expr

 (**
  * [mk_solver ctx] returns a Z3 solver adapted to the analysis of program
  * automata.
  *)
 val mk_solver : Z3.context -> Z3.Solver.solver
 
 (**
  * [mk_zdiv ctx e f] returns a Z3 expression representing the integer division
  * e / f {e rounded towards zero}.  This is indeed the behavior of / in most
  * programming languages (including C).  This is in contrast with the function
  * [Z3.Arithmetic.mk_div] which corresponds to {e floored} integer division.
  * The expression returned by [mk_zdiv ctx e f] is ({b if} ({i e} ≥ 0) {b then}
  * {i e div f} {b else} - ((- {i e}) {i div f})).
  *)
 val mk_zdiv : Z3.context -> t -> t -> t
 
 (**
  * [create_state_functions ctx aut] creates helper functions to manipulate
  * states in Z3 formulas.  The arguments are a Z3 context [ctx] and a program
  * automaton [aut].
  * It returns a triple of functions
  * [(get_expr_from_node, get_node_from_expr, create_variable)] where:
  * - [get_expr_from_node] takes a node of [aut] and returns the constant Z3
  *   expression representing it. Raises [Not_Found] if the argument is not a
  *   node of [aut].
  * - [get_node_from_expr] takes a Z3 expression and returns the node of [aut]
  *   which it represents (if any). Raises [Not_Found] if the argument does not
  *   represent a node of [aut].
  * - [create_variable] takes a string and returns a Z3 variable of type {i "node
  *   of [aut]"} (which is a custom type) with that name.
  *)
 val create_state_functions :
   Z3.context ->
   Automaton.t ->
   (Automaton.Node.t -> Z3.Expr.expr) *
   (Z3.Expr.expr -> Automaton.Node.t) * (string -> Z3.Expr.expr)
 
 
 (**
  * [create_transition_functions ctx aut] creates helper functions to manipulate
  * transitions in Z3 formulas. Here, transition are understood as triples
  * {i (n,c,n')} where {i n} and {i n'} are nodes of [aut] and {i c} is a label.
  * The arguments are a Z3 context [ctx] and a program automaton [aut].
  * It returns a triple of functions
  * [(get_expr_from_transition, get_node_from_expr, create_variable)] where:
  * - [get_expr_from_transition] takes a transition of [aut] and returns the
  *   constant Z3 expression representing it. Raises [Not_Found] if the argument
  *   is not a transition of [aut].
  * - [get_transition_from_expr] takes a Z3 expression and returns the transition
  *   of [aut] which it represents (if any). Raises [Not_Found] if the argument
  *   does not represent a transition of [aut].
  * - [create_variable] takes a string and returns a Z3 variable of type {i
  *   "transition of [aut]"} (which is a custom type) with that name.
  *)
 val create_transition_functions :
   Z3.context ->
   Automaton.t ->
   (Automaton.Node.t * Automaton.Label.t * Automaton.Node.t -> Z3.Expr.expr) *
   (Z3.Expr.expr -> Automaton.Node.t * Automaton.Label.t * Automaton.Node.t) *
   (string -> Z3.Expr.expr)
 