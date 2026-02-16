(* $Id$ *)


(**
   Semantics of program commands.

   This module translates program commands (see {! Command}) into [Z3] formulas.
   The latter are values of type [Z3.Expr.expr].
   The translation of a command {i cmd} is given by the formula 《{i cmd}》 that
   has been presented in the course (for details, refer to the PDF document
   linked below).

   Let us recall that the set of free variables of the formula 《{i cmd}》
   is {i \{x, x' | x ∈ X\}}, where X is the underlying set of all variables.
   Typically, X is the set of variables of the program automaton (see
   {! Automaton}) under analysis.  The implementation of this module encodes
   primes with a dollar sign.  For instance, assuming that [foo] is a variable,
   [foo]' is encoded by [foo$].  This cannot cause confusion since variables of
   program automata contain no dollar sign (the lexers {! AutLexer} and
   {! PrgLexer} enforce this property).

   The formula 《{i cmd}》 corresponds to the execution of the command {i cmd}
   in the standard direction, which is {i forward}.  So primed variables in
   《{i cmd}》 denote the values of the program variables after the execution of
   {i cmd}.  This module also implements a backward translation of program
   commands, where primed variables of the formula stand for the values of the
   program variables before the execution of the command.

   @see <http://www.labri.fr/perso/leroux/cours/lesson-notations.pdf>
     A few notations for the Software Verification course
   @see <http://z3prover.github.io/api/html/ml/Z3.Expr.html> Z3.Expr
 *)


(**
   [fwd_formula ctx cmd] returns a pair [(f, l)] that denotes the forward
   translation of the command [cmd].  The pair [(f, l)] stands for the
   conjunction f ∧ ⋀{_ x ∉ [l]} x' = x.
   {L {b Ensures:} The list [l] contains no repetition.}
 *)
val fwd_formula : Z3.context -> Command.t -> Z3.Expr.expr * Variable.t list

(**
   [bwd_formula ctx cmd] returns a pair [(f, l)] that denotes the backward
   translation of the command [cmd].  The pair [(f, l)] stands for the
   conjunction f ∧ ⋀{_ x ∉ [l]} x' = x.
   {L {b Ensures:} The list [l] contains no repetition.}
 *)
val bwd_formula : Z3.context -> Command.t -> Z3.Expr.expr * Variable.t list
