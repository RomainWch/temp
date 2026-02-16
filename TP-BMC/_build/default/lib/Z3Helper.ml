(* $Id$ *)


(*
   Helper module for Z3 formulas.
 *)


open Z3

type t = Expr.expr

let print fmt f =
  let explode =
    Str.split (Str.regexp "\r?\n[ \t]*")
  in
  (Print.list_printer_from_printer "@ " Format.pp_print_string)
    fmt
    (explode (Expr.to_string f))

let mk_solver ctx =
  let solver = Z3.Solver.mk_simple_solver ctx (* or mk_solver_s ctx "QF_NIA" *)
  and params = Z3.Params.mk_params ctx
  in
  (*
     Use the legacy arithmetic solver since the default new one loops forever,
     disregarding the specified timeout, on some non-linear arithmetic queries
     (especially ones with modulo constraints).
   *)
  Z3.Params.add_int params (Z3.Symbol.mk_string ctx "arith.solver") 2 ;
  Z3.Solver.set_parameters solver params ;
  solver

let mk_zdiv ctx e f =
  let zero = Arithmetic.Integer.mk_numeral_i ctx 0
  in
  Boolean.mk_ite ctx
    (Arithmetic.mk_ge ctx e zero)
    (Arithmetic.mk_div ctx e f)
    (Arithmetic.mk_unary_minus ctx
       (Arithmetic.mk_div ctx (Arithmetic.mk_unary_minus ctx e) f))

let create_state_functions ctx automaton =
	let node_list = Automaton.nodes automaton in
	let sort = Enumeration.mk_sort_s ctx ((Automaton.name automaton)^":states") 
		node_list in
	let expr_list = Enumeration.get_consts sort in
	let name_list = List.map Expr.to_string expr_list in 
	((fun node -> List.assoc node (List.combine node_list expr_list)),
		(fun expr -> let name = Expr.to_string expr in List.assoc name 
		(List.combine name_list node_list)),
		(fun name -> Expr.mk_const_s ctx name sort))


let create_transition_functions ctx automaton =
	let node_list = Automaton.nodes automaton in
	let transition_list = 
		List.flatten (List.map (fun node -> List.map 
		(fun (cmd,tgt) -> (node,cmd,tgt)) (Automaton.succ automaton node))
		node_list) in
	let number_list = List.mapi (fun i _ -> (string_of_int i))
		transition_list in
	let sort = Enumeration.mk_sort_s ctx 
		((Automaton.name automaton)^"transitions") number_list in
	let expr_list = Enumeration.get_consts sort in
	let name_list = List.map Expr.to_string expr_list in
	((fun node -> List.assoc node (List.combine transition_list expr_list)),
		(fun expr -> let name = Expr.to_string expr in List.assoc name 
		(List.combine name_list transition_list)),
		(fun name -> Expr.mk_const_s ctx name sort))
		
		
