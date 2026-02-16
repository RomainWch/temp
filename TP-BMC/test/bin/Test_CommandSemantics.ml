(* $Id$ *)


(*
   Unit tests for the CommandSemantics module.
 *)


open TestCore
open CommandSemantics

(* Import utility functions. *)
open TestUtil.CommandShortcut

(* Helper function to create test messages. *)
let message cmd =
  Format.asprintf "@[<h>[%a]@]" Command.print cmd

module Make () =
struct
let x = var "x"
and y = var "y"
and z = var "z"

let context = Z3.mk_context []

(* Generic test function for the forward translation of program commands. *)
let fwd_aux cmd expected =
  let (f, l) = fwd_formula context cmd
  in
  assert_string
    ~msg:(message cmd)
    expected
    ((Format.asprintf "@[<h>%a@]" Z3Helper.print f) ^ "; [" ^ (String.concat ", " l) ^ "]")

let test_fwd_assign () =
  fwd_aux
    (x := cst 3)
    "(and (= x$ 3)); [x]" ;
  fwd_aux
    (z := z + y)
    "(and (= z$ (+ z y))); [z]" ;
  fwd_aux
    (z := x / y)
    "(let ((a!1 (ite (>= x 0) (div x y) (- (div (- x) y))))) (and (not (= y 0)) (= z$ a!1))); [z]" ;
  fwd_aux
    (z := x * (y - cst 5))
    "(and (= z$ (* x (- y 5)))); [z]" ;
  fwd_aux
    (x := y / (x / z))
    "(let ((a!1 (ite (>= x 0) (div x z) (- (div (- x) z))))) (let ((a!2 (ite (>= y 0) (div y a!1) (- (div (- y) a!1))))) (and (not (= z 0)) (not (= a!1 0)) (= x$ a!2)))); [x]"

let test_fwd_guard () =
  fwd_aux
    (x != y * z)
    "(and (not (= x (* y z)))); []" ;
  fwd_aux
    (x + cst 5 > y - z)
    "(and (> (+ x 5) (- y z))); []" ;
  fwd_aux
    (x <= y / (cst 3 * z))
    "(let ((a!1 (ite (>= y 0) (div y (* 3 z)) (- (div (- y) (* 3 z)))))) (and (not (= (* 3 z) 0)) (<= x a!1))); []" ;
  fwd_aux
    (x / y == cst 1 / z)
    "(let ((a!1 (ite (>= x 0) (div x y) (- (div (- x) y)))) (a!2 (ite (>= 1 0) (div 1 z) (- (div (- 1) z))))) (and (not (= y 0)) (not (= z 0)) (= a!1 a!2))); []"

let test_fwd_skip () =
  fwd_aux (skip) "true; []"

(* Generic test function for the backward translation of program commands. *)
let bwd_aux cmd expected =
  let (f, l) = bwd_formula context cmd
  in
  assert_string
    ~msg:(message cmd)
    expected
    ((Format.asprintf "@[<h>%a@]" Z3Helper.print f) ^ "; [" ^ (String.concat ", " l) ^ "]")

let test_bwd_assign () =
  bwd_aux
    (x := cst 3)
    "(and (= x 3)); [x]" ;
  bwd_aux
    (z := z + y)
    "(and (= z (+ z$ y))); [z]" ;
  bwd_aux
    (z := x / y)
    "(let ((a!1 (ite (>= x 0) (div x y) (- (div (- x) y))))) (and (not (= y 0)) (= z a!1))); [z]" ;
  bwd_aux
    (z := x * (y - cst 5))
    "(and (= z (* x (- y 5)))); [z]" ;
  bwd_aux
    (x := y / (x / z))
    "(let ((a!1 (ite (>= x$ 0) (div x$ z) (- (div (- x$) z))))) (let ((a!2 (ite (>= y 0) (div y a!1) (- (div (- y) a!1))))) (and (not (= z 0)) (not (= a!1 0)) (= x a!2)))); [x]"

let test_bwd_guard () =
  bwd_aux
    (x != y * z)
    "(and (not (= x (* y z)))); []" ;
  bwd_aux
    (x + cst 5 > y - z)
    "(and (> (+ x 5) (- y z))); []" ;
  bwd_aux
    (x <= y / (cst 3 * z))
    "(let ((a!1 (ite (>= y 0) (div y (* 3 z)) (- (div (- y) (* 3 z)))))) (and (not (= (* 3 z) 0)) (<= x a!1))); []" ;
  bwd_aux
    (x / y == cst 1 / z)
    "(let ((a!1 (ite (>= x 0) (div x y) (- (div (- x) y)))) (a!2 (ite (>= 1 0) (div 1 z) (- (div (- 1) z))))) (and (not (= y 0)) (not (= z 0)) (= a!1 a!2))); []"

let test_bwd_skip () =
  bwd_aux (skip) "true; []"

(* Collection of all tests. *)
let tests prefix =
  [
    prefix ^ ".fwd_assign", test_fwd_assign ;
    prefix ^ ".fwd_guard", test_fwd_guard ;
    prefix ^ ".fwd_skip", test_fwd_skip ;
    prefix ^ ".bwd_assign", test_bwd_assign ;
    prefix ^ ".bwd_guard", test_bwd_guard ;
    prefix ^ ".bwd_skip", test_bwd_skip ;
  ]
end

(* This test suite. *)
let suite = ("CommandSemantics", lazy (let open Make () in tests "cmdsemantics"))
