(* $Id$ *)


(*
 * Bounded Model Checking.
 *)


open Z3

module VarMap = Map.Make (Variable)

type result =
  | Path of (Command.t * Automaton.Node.t) list
  | Empty of bool

(* Taken from CommandSemantics.ml. *)
let var_to_z3_expr ctx v =
  Arithmetic.Integer.mk_const_s ctx v

(*
 * Wrapper around CommandSemantics.fwd_formula and CommandSemantics.bwd_formula
 * to translate program commands into formulas with variables indexed by the
 * depth of their last modification.
 *)
let indexed_formula formula ctx map depth cmd =
  assert (VarMap.for_all (fun _ i -> i <= depth) map) ;
  let (f, l) = formula ctx cmd
  in
  (* Map after the command. *)
  let map' =
    List.fold_left
      (fun m v -> VarMap.add v (depth+1) m)
      map
      l
  in
  (* Substitution lists: v ↦ v$map(v) and v$ ↦ v$map'(v). *)
  let (subst_from, subst_to) =
    VarMap.fold
      (fun v i (fromlist, tolist) ->
        ((var_to_z3_expr ctx v) :: fromlist,
         (var_to_z3_expr ctx (v ^ "$" ^ (string_of_int i))) :: tolist))
      map
      ([], [])
  in
  let (subst_from', subst_to') =
    VarMap.fold
      (fun v i (fromlist, tolist) ->
        ((var_to_z3_expr ctx (v ^ "$")) :: fromlist,
         (var_to_z3_expr ctx (v ^ "$" ^ (string_of_int i))) :: tolist))
      map'
      (subst_from, subst_to)
  in
  (Expr.substitute f subst_from' subst_to', map')


(*
 * Notation for the functions that will convert the step at a given depth in the
 * execution exploration into the formula where the variables have the right
 * number of $. It simply calls the module CommandSemantics, and renames the
 * variables as needed.
 *)
let fwd_formula_maker = indexed_formula CommandSemantics.fwd_formula
let bwd_formula_maker = indexed_formula CommandSemantics.bwd_formula

(*
 * The types and roles of the arguments of dfs are the following:
 *
 * - formula_maker: Z3.context -> int VarMap.t -> int -> Command.t ->
 *   (Z3.Expr.expr * int VarMap.t). It is a /function/ which takes a Z3 context
 *   (the usual context you put everywhere), a mapping from variables of the
 *   program to their Z3 representation at the current depth (to know what names
 *   the result formula needs to manipulate to faithfully represent the program
 *   flow), an integer representing the current depth, and the command you want
 *   to get the formula of. It returns a pair containing a Z3 formula
 *   representing the command you passed in argument (dependent on the current
 *   depth), and the updated current name of variables /after/ the step
 *   represented by the formula.
 *
 * - ctx: Z3.context. The usual Z3 context you put everywhere Z3 is called.
 *
 * - solver: Z3.Solver.solver. The Z3 solver that will solve the formula you
 *   gave it. You will use it by passing it the formula of the current location
 *   and call recursively the dfs. When returning from the exploration of a
 *   child, you'll pop the formula added on this child to have only the path to
 *   the current location in the solver (see the add, push and pop functions in
 *   Z3.Solver).
 *
 * - automaton: Automaton.t. The program automaton (or rather its transition
 *   system) you are verifying.
 *
 * - bound: int. The depth bound for the dfs. If you reach it, you stop the
 *   exploration.
 *
 * - loc: Automaton.Node.t. The current node of the automaton you are in. Used
 *   to know what are the possible step to take next.
 *
 * - map: int VarMap.t. The current names of the variables at the current
 *   location (for the Z3 formula). Only needed by formula_maker, and updated by
 *   it. For your information, the name of a variable will be x$i, where i is
 *   the depth of its last modification. E.g., if the variable x was modified at
 *   steps 2,4 and 7, and we're at position 10, its name will be x$7.
 *
 * - depth: int. The depth of the current location on our path. Used to test if
 *   we have reached the bound or not.
 *
 * - verbose: boolean. Toggles verbose information about the exploration (set by
 *   command line).
 *
 * The result of the dfs is of type result (defined above). Empty true means no
 * counter-example was found and the search was exhaustive, Empty false
 * similarly, but the search was not exhaustive (bound reached), and Path list
 * means that list is an execution from the location in argument to the final
 * state.
 *
 * The dfs will do the following steps:
 *
 * 1. Check if depth > bound. If so return that it didn't find a counter-example
 *    (but the search is not exhaustive), i.e., Empty false.
 *
 * 2. Check if the path is an actual execution. To do so call the function check
 *    on the solver and put the check in a constant (hereafter named status). If
 *    the path is not executable (i.e., status = UNSATISFIABLE) returns it has
 *    no counter-example, and exhaustive search (for the current branch), i.e.,
 *    Empty true. Otherwise, we'll now continue the dfs.
 *
 * 3. If we are in the final state (bad state), checks if status is SATISFIABLE
 *    or UNKNOWN. If satisfiable, return you have a counter example (Path [],
 *    you'll construct the path on the way up), if not, return you have no
 *    counter-example (but we're not exhaustive, as we are not sure we actually
 *    have reached the bad state), i.e., Empty false.
 *
 * 4. If we are not in a final state, we now do our recursion to explore the
 *    tree of executions from the current location. You will put all the
 *    possible transitions from the current location in a list (obtainable with
 *    the function Automaton.succ), and, successively on each element on the
 *    list, you will explore that element (if you didn't already find a
 *    counter-example) by calling dfs on it (with the right argument), and pass
 *    the result to the next element (if you get a counter-example as a result
 *    of the dfs, you add the explored transition in front of the
 *    counter-example path, otherwise, you pass the Empty b you got). If you get
 *    a counter-example from your sibling, you do not explore the current child
 *    and just pass the counter-example to the next element (or your father if
 *    you are the last element).
 *
 * Formally, to do the previous, you will declare an auxiliary function [taking
 * a result and a transition (here, a command and a node - the target of the
 * transition) which yields the result of the exploration of target of the
 * transition passed in argument], and apply it successively to the list of
 * successors using List.fold_left. This function will allow to pass the result
 * of a call to the next one (look at the documentation for the List module).
 *)

let rec dfs (formula_maker : context -> int VarMap.t -> int -> Command.t ->
  (Z3.Expr.expr * int VarMap.t)) (ctx: context) (solver : Solver.solver) (automaton : Automaton.t) (bound : int) (loc : Automaton.Node.t) (map : int VarMap.t) (depth : int) (verbose : bool) =
  (*1.*)
  if depth > bound then Empty false else 
    let status = Solver.check solver [] in 
      (*2. not an actual execution*)
      if status == UNSATISFIABLE then Empty true else 
        (*3. in final state *)
        if loc == Automaton.final automaton then 
          if status == UNKNOWN then Empty false 
            else Path []
          (*4. not in final state*)
          else 
            let transitions = Automaton.succ automaton loc in 
            let rec aux res (cmd, node) = 
              match res with 
              | Path _ -> res
              | Empty _ -> let (f, map') = formula_maker ctx map depth cmd in 
                  Solver.push solver;
                  Solver.add solver [f];
                  let res = dfs formula_maker ctx solver automaton bound node map' (depth + 1) verbose in 
                  Solver.pop solver 1;
                  match res with 
                  | Path path -> Path ((cmd, node) :: path)
                  | Empty b -> Empty b
            in List.fold_left aux (Empty true) transitions


(*
 * Creates the initial map of the variables name of the automaton. Simply maps
 * all variables to 0.
 *)
let initial_map automaton =
  List.fold_left
    (fun m v -> VarMap.add v 0 m)
    VarMap.empty
    (Automaton.variables automaton)

let fwd_search automaton ctx bound verbose =
  assert (bound >= 0) ;
  let solver = Z3Helper.mk_solver ctx
  in
  let res =
    dfs
      fwd_formula_maker ctx solver
      automaton bound (Automaton.initial automaton)
      (initial_map automaton) 0 verbose
  in res

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

let bwd_search automaton ctx bound verbose =
  assert (bound >= 0) ;
  let solver = Z3Helper.mk_solver ctx
  in
  let res =
    dfs
      bwd_formula_maker ctx solver
      (Automaton.reverse automaton) bound (Automaton.final automaton)
      (initial_map automaton) 0 verbose
  in reverse_result automaton res
