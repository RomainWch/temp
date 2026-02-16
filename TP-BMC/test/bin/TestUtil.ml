(* $Id$ *)


(*
   Utility functions for unit tests.
 *)


module CommandShortcut =
struct
  (* Shortcuts to write expressions. *)
  let cst c = Command.Expression.Cst c
  let var v = Command.Expression.Var v
  let ( + ) e e' = Command.Expression.Op (e, Command.Expression.Add, e')
  let ( - ) e e' = Command.Expression.Op (e, Command.Expression.Sub, e')
  let ( * ) e e' = Command.Expression.Op (e, Command.Expression.Mul, e')
  let ( / ) e e' = Command.Expression.Op (e, Command.Expression.Div, e')

  (* Shortcuts to write guards. *)
  let ( == ) e e' = Command.Guard (e, Command.Predicate.Eq, e')
  let ( < ) e e' = Command.Guard (e, Command.Predicate.Lst, e')
  let ( > ) e e' = Command.Guard (e, Command.Predicate.Gst, e')
  let ( <= ) e e' = Command.Guard (e, Command.Predicate.Leq, e')
  let ( >= ) e e' = Command.Guard (e, Command.Predicate.Geq, e')
  let ( != ) e e' = Command.Guard (e, Command.Predicate.Neq, e')

  (* Shortcut to write assignments. *)
  let ( := ) v e =
    match v with
    | Command.Expression.Var v -> Command.Assign (v, e)
    | Command.Expression.Cst _
    | Command.Expression.Op _ -> invalid_arg "TestUtil.OfAbstractDomain.(:=)"

  (* Shortcut to write skips. *)
  let skip = Command.Skip
end
