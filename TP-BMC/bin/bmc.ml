(* $Id$ *)


(*
   Main (start of the bmc program).
 *)


(* Exception for errors that are local to this module (should not happen). *)
exception Internal_error

(* Module to parse the command-line and store user-specified options. *)
module CommandLine =
struct
  (* Input format. *)
  let format_opt = ref "auto"

  (* Direction of the analysis.  Either "fwd" or "bwd". *)
  let analysis_opt = ref "fwd"

  (* Bounded model-checking algorithm.  Either "dfs" or "global". *)
  let algorithm_opt = ref "dfs"

  (* Bound on the length of paths. *)
  let bound_opt = ref 10

  (* Z3 timeout. *)
  let z3_timeout_opt = ref 5000

  (* Verbosity switch. *)
  let verbosity_opt = ref false

  (* Specification of command-line options. *)
  let arg_spec_list = [
    ("-format",
     Arg.Symbol
       (["auto" ; "aut" ; "prg"], fun s -> format_opt := s),
     " input format" ^
       " (default: " ^ !format_opt ^ ")")
    ;
    ("-analysis",
     Arg.Symbol
       (["fwd" ; "bwd"], fun s -> analysis_opt := s),
     " direction of the analysis" ^
       " (default: " ^ !analysis_opt ^ ")")
    ;
    ("-algo",
     Arg.Symbol
       (["dfs" ; "global"], fun s -> algorithm_opt := s),
     " bounded model-checking algorithm" ^
       " (default: " ^ !algorithm_opt ^ ")")
    ;
    ("-bound",
     Arg.Int
       (fun n ->
         if n >= 0 then bound_opt := n
         else
           raise (Arg.Bad ("Incorrect negative value `" ^ (string_of_int n) ^
                             "' for option -bound"))),
     "<int> bound on the length of paths" ^
       " (default: " ^ (string_of_int !bound_opt) ^ ")")
    ;
    ("-z3-timeout",
     Arg.Int
       (fun n ->
         if n >= 0 then z3_timeout_opt := n
         else
           raise (Arg.Bad ("Incorrect negative value `" ^ (string_of_int n) ^
                             "' for option -z3-timeout"))),
     "<int> timeout in milliseconds for Z3 solving" ^
       " (default: " ^ (string_of_int !z3_timeout_opt) ^ ")")
    ;
    ("-v",
     Arg.Set verbosity_opt,
     " display the explored path" ^
       " (default: " ^ (string_of_bool !verbosity_opt) ^ ")")
  ]

  let usage_msg = "Usage: " ^ (Sys.argv.(0)) ^ " [option ...] [source-file]\n"

  (* Parses the command line and returns the input file's name, if any. *)
  let parse () =
    let filename = ref None
    in
    Arg.parse
      (Arg.align arg_spec_list)
      (fun a ->
       match !filename with
       | None ->
          filename := Some a
       | Some _ ->
          raise (Arg.Bad ("unexpected argument `" ^ a ^
                            "' (multiple input files are not allowed)")))
      usage_msg ;
    !filename
end

(* Default number of columns for pretty-printing. *)
let () =
  try
    Format.set_margin (int_of_string (Sys.getenv "COLUMNS"))
  with
    Not_found | Failure _ -> Format.set_margin 78

(* Parse the command line. *)
let filename = CommandLine.parse ()

(* Desired format. *)
let format =
  match !CommandLine.format_opt with
  | "aut" -> Automaton.Aut
  | "prg" -> Automaton.Prg
  | "auto" ->
     begin
       let (fn, ext) =
         match filename with
         | None -> ("<stdin>", "")
         | Some f -> (f, Filename.extension f)
       in
       match ext with
       | ".aut" -> Automaton.Aut
       | ".c" | ".prg" -> Automaton.Prg
       | _ ->
          let msg = "unable to detect the input format; use option -format"
          in
          Format.eprintf "@[%s:@ %s@]@." fn msg ;
          exit 1
     end
  | _ -> raise Internal_error

(* Parse the input file. *)
let automaton =
  let (ic, fn) =
    match filename with
    | None -> (stdin, "<stdin>")
    | Some f -> (open_in f, f)
  in
  let a =
    try
      Automaton.read format ic
    with IO.Read_error msg ->
      Format.eprintf "@[%s:@ %s@]@." fn msg ;
      close_in ic ;
      exit 1
  in
  close_in ic ;
  a

(* Create a Z3 context to be used for all calls to Z3 functions. *)
let ctx = Z3.mk_context [("timeout", string_of_int !CommandLine.z3_timeout_opt)]

(* Create the bounded model-checker. *)
let bounded_model_checker : (module BoundedModelChecking.S) =
  match !CommandLine.algorithm_opt with
  | "dfs"    -> (module DepthFirstBMC)
  | "global" -> (module GlobalBMC)
  | _ -> raise Internal_error

module BMC = (val bounded_model_checker : BoundedModelChecking.S)

(* Account for the user-specified direction of the analysis. *)
let (search, analysis_name) =
  match !CommandLine.analysis_opt with
  | "fwd" -> (BMC.fwd_search, "forward")
  | "bwd" -> (BMC.bwd_search, "backward")
  | _ -> raise Internal_error

(* Print the automaton. *)
let () =
  Format.printf "@[%a@]@." Automaton.print automaton

(* Print the selected analysis options. *)
let () =
  Format.printf
    "@.@[Bounded model checking: %s, depth <= %d@]@."
    analysis_name
    !CommandLine.bound_opt

(* Perform bounded model checking. *)
let result =
  search automaton ctx (!CommandLine.bound_opt) (!CommandLine.verbosity_opt)

(* Display the result. *)
let () =
  Format.printf
    "@.@[<v 3>Feasible path" ;
  begin
    match result with
    | BMC.Path path ->
       Format.printf
         " (of length %d):@,@[<v 3>%a"
         (List.length path)
         Automaton.Node.print
         (Automaton.initial automaton) ;
       List.iter
         (fun (cmd, loc) ->
           Format.printf
             "@,@[<h>»@ @[%a@]@ »@]@]@,@[<v 3>%a"
             Command.print cmd
             Automaton.Node.print loc)
         path ;
       Format.printf "@]"
    | BMC.Empty b ->
       Format.printf
         ":@,@[None,@ %sexhaustive@]" (if b then "" else "not ")
  end ;
  Format.printf "@]@."

(* Display the reachability status. *)
let () =
  let status =
    match result with
    | BMC.Path _ -> "Yes"
    | BMC.Empty true -> "No"
    | BMC.Empty false -> "Unknown"
  in
  Format.printf
    "@.@[Reachability of final location %a from initial location %a: %s@]@."
    Automaton.Node.print (Automaton.final automaton)
    Automaton.Node.print (Automaton.initial automaton)
    status

(* Exit. *)
let () = exit 0
