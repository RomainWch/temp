(* $Id$ *)


(*
   Program Variables.
 *)


type t = string
let print = Format.pp_print_string
let compare = compare
let equal = (=)
let hash = Hashtbl.hash
