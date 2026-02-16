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
      let q_generic = var_node "q" in
      let q_prime_generic = var_node "q'" in
      let q_depth = var_node ("q$" ^ (string_of_int depth)) in
      let q_depth_next = var_node ("q$" ^ (string_of_int (depth + 1))) in
      let renamed_formula = Z3.Expr.substitute formula [q_generic; q_prime_generic] [q_depth; q_depth_next] in
      Solver.add solver [renamed_formula];
      let status = Solver.check solver [] in
      if status == Solver.UNSATISFIABLE then Empty true else
        let goal_expr = node_assoc goal in
        Solver.push solver;
        Solver.add solver [Boolean.mk_eq ctx q_depth_next goal_expr];
        let status_goal = Solver.check solver [] in
        if status_goal == Solver.SATISFIABLE then begin
          let path = [] in
          Solver.pop solver 1;
          Path path
        end else begin
          Solver.pop solver 1;
          globalBmc formula ctx solver automaton var_node node_assoc trans_get var_trans goal bound (depth + 1) verbose
        end


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
  let solver = Z3Helper.mk_solver ctx in
  let (node_assoc, _node_get, var_node) = Z3Helper.create_state_functions ctx automaton in
  let (_trans_assoc, trans_get, var_trans) = Z3Helper.create_transition_functions ctx automaton in
  let node_list = Automaton.nodes automaton in
  let transitions = List.flatten (List.map (fun node -> 
    List.map (fun (cmd, tgt) -> (node, cmd, tgt)) (Automaton.succ automaton node)
  ) node_list) in
  let q = var_node "q" in
  let q' = var_node "q'" in
  let step_formula = 
    let transition_formulas = List.map (fun (src, cmd, tgt) ->
      let (cmd_formula, _) = CommandSemantics.fwd_formula ctx cmd in
      Boolean.mk_and ctx [
        Boolean.mk_eq ctx q (node_assoc src);
        Boolean.mk_eq ctx q' (node_assoc tgt);
        cmd_formula
      ]
    ) transitions in
    Boolean.mk_or ctx transition_formulas
  in
  let q0 = var_node "q$0" in
  let initial_node = Automaton.initial automaton in
  Solver.add solver [Boolean.mk_eq ctx q0 (node_assoc initial_node)];
  let goal = Automaton.final automaton in
  let res = globalBmc step_formula ctx solver automaton var_node node_assoc trans_get var_trans goal bound 0 verbose in
  res


(* Helper function to reverse a result. Used in bwd_search. *)
let reverse_result automaton res =
  let rec reverse_path acc src =
    function
    | [] -> (src, acc)
    | (cmd, tgt) :: tail -> reverse_path ((cmd, src) :: acc) tgt tail
  in
  match res with
  | Path path ->
     let (src, revpath) = (reverse_path [] (Automaton.final automaton) path)
     in
     assert (Automaton.Node.equal src (Automaton.initial automaton)) ;
     Path revpath
  | Empty _ -> res

(* Calls the global BMC in backward.
 *
 * val bwd_search : Automaton.t -> Z3.context -> int -> bool -> result
 *
 * Same as above, but uses the reverse of Automaton, and needs to reverse the obtained counter-example if there is one.
 *)

let bwd_search (automaton : Automaton.t) (ctx : context) (bound : int) (verbose : bool) =
  assert (bound >= 0) ;
  let rev_automaton = Automaton.reverse automaton in
  let solver = Z3Helper.mk_solver ctx in
  let (node_assoc, _node_get, var_node) = Z3Helper.create_state_functions ctx rev_automaton in
  let (_trans_assoc, trans_get, var_trans) = Z3Helper.create_transition_functions ctx rev_automaton in
  let node_list = Automaton.nodes rev_automaton in
  let transitions = List.flatten (List.map (fun node -> 
    List.map (fun (cmd, tgt) -> (node, cmd, tgt)) (Automaton.succ rev_automaton node)
  ) node_list) in
  let q = var_node "q" in
  let q' = var_node "q'" in
  let step_formula = 
    let transition_formulas = List.map (fun (src, cmd, tgt) ->
      let (cmd_formula, _) = CommandSemantics.bwd_formula ctx cmd in
      Boolean.mk_and ctx [
        Boolean.mk_eq ctx q (node_assoc src);
        Boolean.mk_eq ctx q' (node_assoc tgt);
        cmd_formula
      ]
    ) transitions in
    Boolean.mk_or ctx transition_formulas
  in
  let q0 = var_node "q$0" in
  let start_node = Automaton.initial rev_automaton in 
  Solver.add solver [Boolean.mk_eq ctx q0 (node_assoc start_node)];
  let goal = Automaton.final rev_automaton in 
  let res = globalBmc step_formula ctx solver rev_automaton var_node node_assoc trans_get var_trans goal bound 0 verbose in
  reverse_result automaton res