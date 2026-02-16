(* $Id$*)

open Z3

type result =
  | Path of (Command.t * Automaton.Node.t) list
  | Empty of bool


(*The globalBmc 
 *
 * val globalBmc :
  Z3.Expr.expr ->
  Z3.context ->
  Z3.Solver.solver ->
  Automaton.t ->
  (string -> Z3.Expr.expr) ->
  (string -> Z3.Expr.expr) ->
  (Z3.Expr.expr -> Automaton.t * Command.t * Automaton.Node.t) ->
  (string -> Z3.Expr.expr) -> string -> int -> int -> bool -> result
 *
 * - [formula] : Z3.Expr.expr is the step formula of the automaton.
 * - [ctx] : Z3.context is the context of the solver.
 * - [solver] : Z3.Solver.solver is the solver.
 * - [automaton] : Automaton.t is the CFA whose we are verifying
 * - [var_node] : (string -> Z3.Expr.expr) is a function which takes a string and creates a variable of that name that can take nodes of [automaton] as value. See Z3Helper for more detail.
 * - [node_assoc] : (string -> Z3.Expr.expr) is a function which takes a node of [automaton]  and return a constant expression representing it. See Z3Helper for more detail.
 * - [trans_get] : (Z3.Expr.expr -> Automaton.t * Command.t * Automaton.Node.t) is a function which, given an constant expression representing a transition return this transition. See Z3Helper for more detail.
 * - [var_trans] : (string -> Z3.Expr.expr)  is a function which takes a string and creates a variable of that name that can take transitions of [automaton] as value. See Z3Helper for more detail.
 * - [goal] : string is the node we are trying to reach (or prove unreachable), i.e., q_{bad} in a forward search, q_{in} in a backward search.
 * - [bound] : int is the bound of the BMC.
 * - [depth] : int is the current depth of the search.
 * - [verbose] : bool is a switch to toggle verbosity of the search.
 *
 * This is a recursive algorithm. If the bound is exceeded ([bound > depth]), the search is non exhaustive, and we return [Empty false].
 * Otherwise, we create the step formula of the current depth (through renaming [formula]), we add it to the solver, and check if the whole is satisfiable. 
  If not, no execution of depth [depth] exists, and we return an exhaustive search, i.e., [Empty true]. If so, we add a formula asserting that [goal] is reached at depth [depth]. 
 If that last formula is satisfiable, we found a counter example we can retrieve from the model. We thus return [Path path], where [path] is the counter example.
 Otherwise, we continue the search at next depth by recursively calling the function at next depth (keeping the step formula in the solver, but not the one forcing [goal] to be reached).
 *)
let rec globalBmc (formula : Expr.expr) (ctx : context) (solver : Solver.solver) (automaton : Automaton.t)
(var_node : string -> Expr.expr) (node_assoc : Automaton.Node.t -> Expr.expr) (trans_get : Expr.expr -> Automaton.Node.t * Command.t * Automaton.Node.t) (var_trans : string -> Expr.expr) (goal : Automaton.Node.t) (bound : int) (depth : int) (verbose : bool) =
    (* Advice : Start with a version which does not handle the retrieving of the counter-example, and deal with that later.
    You need to decide of a name for variables at a given depth and modify the formula accordingly (use an auxiliary function that itself uses Z3.Expr.substitute - note that $ never appears in a variable name but is accepted by Z3 for example).*)
	  if depth > bound then Empty false else 
      let status = Solver.check solver [formula] in
      if status == UNSATISFIABLE then Empty true else
        let qi_expr = var_node ("q_" ^ (string_of_int depth)) in
        let goal_expr = node_assoc goal in
        Solver.push solver;
        Solver.add solver [Boolean.mk_eq ctx qi_expr goal_expr];
        let status = Solver.check solver [] in
        if status == SATISFIABLE then
          Path []
        else
          globalBmc formula ctx solver automaton var_node node_assoc trans_get var_trans goal bound (depth + 1) verbose


(* Calls the global BMC in forward
 *
 * val fwd_search : Automaton.t -> Z3.context -> int -> bool -> result
 *
 * - [automaton] : Automaton.t : the automaton to explore
 * - [bound] : int : the bound of the BMC.
 * - [verbose] : bool : switch to display information during the computation
 *
 * It creates the information for the global algorithm (a solver context, Z3 types representing nodes and transitions, the step formula of the automaton, and then calls the global algorithm with these information.)
 *)
let fwd_search (automaton : Automaton.t) (ctx : context) (bound : int) (verbose : bool) =
  assert (bound >= 0) ;
  (* Advice : First create a solver, and functions to handle states and transitions (see Z3Helper).
  Then create the step formula (it is advisable to delegate it to a auxiliary function you may use for the backward version).
  Finally, call GlobalBMC (don't forget to assert the initial state is q_{in} in the solver).
   *)
  Format.printf "@[I didn't explore anything! Change that!@]@.";
	Empty false


(* Calls the global BMC in backward.
 *
 * val bwd_search : Automaton.t -> Z3.context -> int -> bool -> result
 *
 * Same as above, but uses the reverse of Automaton, and needs to reverse the obtained counter-example if there is one.
 *)
let bwd_search (automaton : Automaton.t) (ctx : context) (bound : int) (verbose : bool) =
  assert (bound >= 0) ;
  (* It is very similar to the previous function, but don't forget to reverse the automaton AND the counter-example*)
    Format.printf "@[I didn't explore anything! Change that!@]@.";
	Empty false