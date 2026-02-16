(* $Id$ *)


(*
   Semantics of program commands.
 *)


open Z3

let cst_to_z3_expr ctx c =
  Arithmetic.Integer.mk_numeral_i ctx c

let var_to_z3_expr ctx v =
  Arithmetic.Integer.mk_const_s ctx v

(*
   Translation of an expression into a Z3 expression with side conditions.

   val expr_to_z3_expr : Z3.context -> Command.Expression.t ->
     Z3.Expr.expr list * Z3.Expr.expr

   expr_to_z3_expr ctx e returns a pair (pre, ze) where ze is the direct
   translation of the expression e into a Z3 expression, and pre is a list of
   side conditions (to forbid division by zero) for the evaluation of the
   expression e.  This is needed because, according to the Z3 tutorial,
   division by zero is allowed in Z3, but the result is not specified.

   Note: to faithfully model C-like division, we use Z3Helper.mk_zdiv instead of
   Z3.Arithmetic.mk_div (the latter models Euclidian division).
 *)
let rec expr_to_z3_expr ctx =
  function
  | Command.Expression.Cst c ->
     ([], cst_to_z3_expr ctx c)
  | Command.Expression.Var v ->
     ([], var_to_z3_expr ctx v)
  | Command.Expression.Op (e, o, e') ->
     let (pre, ze) = expr_to_z3_expr ctx e
     and (pre', ze') = expr_to_z3_expr ctx e'
     in
     let pre'' = pre' @ pre
     in
     match o with
     | Command.Expression.Add -> (pre'', Arithmetic.mk_add ctx [ze ; ze'])
     | Command.Expression.Sub -> (pre'', Arithmetic.mk_sub ctx [ze ; ze'])
     | Command.Expression.Mul -> (pre'', Arithmetic.mk_mul ctx [ze ; ze'])
     | Command.Expression.Div ->
        let p = Boolean.mk_not ctx (Boolean.mk_eq ctx ze' (cst_to_z3_expr ctx 0))
        in
        (p :: pre'', Z3Helper.mk_zdiv ctx ze ze')

(*
   Forward translation of an assignment.

   val fwd_formula_of_assign : Z3.context -> Variable.t ->
     Command.Expression.t -> Z3.Expr.expr * Variable.t list
 *)
let fwd_formula_of_assign ctx v e =
  let (pre, ze) = expr_to_z3_expr ctx e
  in
  (Boolean.mk_and
     ctx
     (List.rev
        ((Boolean.mk_eq
            ctx
            (var_to_z3_expr ctx (v ^ "$"))
            ze) :: pre)),
   [v])

(*
   Backward translation of an assignment.

   val bwd_formula_of_assign : Z3.context -> Variable.t ->
     Command.Expression.t -> Z3.Expr.expr * Variable.t list
 *)
let bwd_formula_of_assign ctx v e =
  let var_v = var_to_z3_expr ctx v
  and var_v' = var_to_z3_expr ctx (v ^ "$")
  and (f, l) = fwd_formula_of_assign ctx v e
  in
  (Expr.substitute f [var_v ; var_v'] [var_v' ; var_v], l)

(*
   Translation of a no-op instruction.

   val formula_of_skip : Z3.context -> Z3.Expr.expr * Variable.t list
 *)
let formula_of_skip ctx = (Boolean.mk_true ctx, [])

(*
   Translation of a guard.

   val formula_of_guard : Z3.context ->
     Command.Expression.t * Command.Predicate.op * Command.Expression.t ->
     Z3.Expr.expr * Variable.t list
 *)
let formula_of_guard ctx (e, o, e') =
  let (pre, ze) = expr_to_z3_expr ctx e
  and (pre', ze') = expr_to_z3_expr ctx e'
  and fun_op =
    match o with
    | Command.Predicate.Eq -> Boolean.mk_eq
    | Command.Predicate.Lst -> Arithmetic.mk_lt
    | Command.Predicate.Gst -> Arithmetic.mk_gt
    | Command.Predicate.Leq -> Arithmetic.mk_le
    | Command.Predicate.Geq -> Arithmetic.mk_ge
    | Command.Predicate.Neq ->
       (fun ctx s t -> Boolean.mk_not ctx (Boolean.mk_eq ctx s t))
  in
  (Boolean.mk_and
     ctx
     (List.rev
        ((fun_op ctx ze ze') :: (pre' @ pre))),
   [])

let fwd_formula ctx =
  function
  | Command.Assign (v, e) -> fwd_formula_of_assign ctx v e
  | Command.Guard p -> formula_of_guard ctx p
  | Command.Skip -> formula_of_skip ctx

let bwd_formula ctx =
  function
  | Command.Assign (v, e) -> bwd_formula_of_assign ctx v e
  | Command.Guard p -> formula_of_guard ctx p
  | Command.Skip -> formula_of_skip ctx
