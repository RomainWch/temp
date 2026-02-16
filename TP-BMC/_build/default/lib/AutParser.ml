
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | TK_VAR
    | TK_SUB
    | TK_STR of (
# 45 "lib/AutParser.mly"
       (string)
# 17 "lib/AutParser.ml"
  )
    | TK_SKIP
    | TK_SEMICOLON
    | TK_RPAREN
    | TK_RBRACE
    | TK_NEQ
    | TK_NAT of (
# 44 "lib/AutParser.mly"
       (int)
# 27 "lib/AutParser.ml"
  )
    | TK_MUL
    | TK_LST
    | TK_LPAREN
    | TK_LEQ
    | TK_LBRACE
    | TK_INITIAL
    | TK_GST
    | TK_GEQ
    | TK_FROM
    | TK_FINAL
    | TK_EQ
    | TK_EOF
    | TK_DIV
    | TK_COMMA
    | TK_CHOICE
    | TK_ASSIGN
    | TK_ARROW
    | TK_ADD
  
end

include MenhirBasics

# 3 "lib/AutParser.mly"
  
  let merge_declaration (vars, init, final, trans) decl =
    match decl with
    | `Variables vl -> (vars @ vl, init, final, trans)
    | `Initial loc -> (vars, Some loc, final, trans)
    | `Final loc -> (vars, init, Some loc, trans)
    | `Location (loc, out) -> (vars, init, final, (loc, out) :: trans)

  let output name decls =
    let (vars, init, final, trans) =
      List.fold_left
        merge_declaration
        ([], None, None, [])
        decls
    in
    (name, vars, init, final, trans)

# 70 "lib/AutParser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState02 : ('s _menhir_cell0_TK_STR, _menhir_box_main) _menhir_state
    (** State 02.
        Stack shape : TK_STR.
        Start symbol: main. *)

  | MenhirState04 : (('s, _menhir_box_main) _menhir_cell1_TK_VAR _menhir_cell0_TK_STR, _menhir_box_main) _menhir_state
    (** State 04.
        Stack shape : TK_VAR TK_STR.
        Start symbol: main. *)

  | MenhirState09 : (('s, _menhir_box_main) _menhir_cell1_comma_string, _menhir_box_main) _menhir_state
    (** State 09.
        Stack shape : comma_string.
        Start symbol: main. *)

  | MenhirState15 : (('s, _menhir_box_main) _menhir_cell1_TK_FROM _menhir_cell0_TK_STR, _menhir_box_main) _menhir_state
    (** State 15.
        Stack shape : TK_FROM TK_STR.
        Start symbol: main. *)

  | MenhirState16 : (('s, _menhir_box_main) _menhir_cell1_TK_CHOICE, _menhir_box_main) _menhir_state
    (** State 16.
        Stack shape : TK_CHOICE.
        Start symbol: main. *)

  | MenhirState17 : (('s, _menhir_box_main) _menhir_cell1_TK_SUB, _menhir_box_main) _menhir_state
    (** State 17.
        Stack shape : TK_SUB.
        Start symbol: main. *)

  | MenhirState20 : (('s, _menhir_box_main) _menhir_cell1_TK_LPAREN, _menhir_box_main) _menhir_state
    (** State 20.
        Stack shape : TK_LPAREN.
        Start symbol: main. *)

  | MenhirState22 : (('s, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 22.
        Stack shape : expression.
        Start symbol: main. *)

  | MenhirState24 : (('s, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 24.
        Stack shape : expression.
        Start symbol: main. *)

  | MenhirState26 : (('s, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 26.
        Stack shape : expression.
        Start symbol: main. *)

  | MenhirState29 : (('s, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 29.
        Stack shape : expression.
        Start symbol: main. *)

  | MenhirState33 : ((('s, _menhir_box_main) _menhir_cell1_TK_CHOICE, _menhir_box_main) _menhir_cell1_TK_STR, _menhir_box_main) _menhir_state
    (** State 33.
        Stack shape : TK_CHOICE TK_STR.
        Start symbol: main. *)

  | MenhirState45 : ((('s, _menhir_box_main) _menhir_cell1_TK_CHOICE, _menhir_box_main) _menhir_cell1_expression _menhir_cell0_relop, _menhir_box_main) _menhir_state
    (** State 45.
        Stack shape : TK_CHOICE expression relop.
        Start symbol: main. *)

  | MenhirState52 : (('s, _menhir_box_main) _menhir_cell1_transition, _menhir_box_main) _menhir_state
    (** State 52.
        Stack shape : transition.
        Start symbol: main. *)

  | MenhirState65 : (('s, _menhir_box_main) _menhir_cell1_declaration, _menhir_box_main) _menhir_state
    (** State 65.
        Stack shape : declaration.
        Start symbol: main. *)


and ('s, 'r) _menhir_cell1_comma_string = 
  | MenhirCell1_comma_string of 's * ('s, 'r) _menhir_state * (string)

and ('s, 'r) _menhir_cell1_declaration = 
  | MenhirCell1_declaration of 's * ('s, 'r) _menhir_state * ([ `Final of string
  | `Initial of string
  | `Location of string * (Command.t * string) list
  | `Variables of string list ])

and ('s, 'r) _menhir_cell1_expression = 
  | MenhirCell1_expression of 's * ('s, 'r) _menhir_state * (Command.Expression.t)

and 's _menhir_cell0_relop = 
  | MenhirCell0_relop of 's * (Command.Predicate.op)

and ('s, 'r) _menhir_cell1_transition = 
  | MenhirCell1_transition of 's * ('s, 'r) _menhir_state * (Command.t * string)

and ('s, 'r) _menhir_cell1_TK_CHOICE = 
  | MenhirCell1_TK_CHOICE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_TK_FROM = 
  | MenhirCell1_TK_FROM of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_TK_LPAREN = 
  | MenhirCell1_TK_LPAREN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_TK_STR = 
  | MenhirCell1_TK_STR of 's * ('s, 'r) _menhir_state * (
# 45 "lib/AutParser.mly"
       (string)
# 180 "lib/AutParser.ml"
)

and 's _menhir_cell0_TK_STR = 
  | MenhirCell0_TK_STR of 's * (
# 45 "lib/AutParser.mly"
       (string)
# 187 "lib/AutParser.ml"
)

and ('s, 'r) _menhir_cell1_TK_SUB = 
  | MenhirCell1_TK_SUB of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_TK_VAR = 
  | MenhirCell1_TK_VAR of 's * ('s, 'r) _menhir_state

and _menhir_box_main = 
  | MenhirBox_main of (string * string list * string option * string option *
  (string * (Command.t * string) list) list) [@@unboxed]

let _menhir_action_01 =
  fun _1 _3 ->
    (
# 110 "lib/AutParser.mly"
  ( Command.Assign (_1, _3) )
# 205 "lib/AutParser.ml"
     : (Command.t))

let _menhir_action_02 =
  fun _2 ->
    (
# 85 "lib/AutParser.mly"
                        ( _2 )
# 213 "lib/AutParser.ml"
     : (string))

let _menhir_action_03 =
  fun _1 ->
    (
# 104 "lib/AutParser.mly"
                        ( _1 )
# 221 "lib/AutParser.ml"
     : (Command.t))

let _menhir_action_04 =
  fun _1 ->
    (
# 105 "lib/AutParser.mly"
                        ( _1 )
# 229 "lib/AutParser.ml"
     : (Command.t))

let _menhir_action_05 =
  fun _1 ->
    (
# 106 "lib/AutParser.mly"
                        ( _1 )
# 237 "lib/AutParser.ml"
     : (Command.t))

let _menhir_action_06 =
  fun _1 ->
    (
# 75 "lib/AutParser.mly"
                        ( `Variables _1 )
# 245 "lib/AutParser.ml"
     : ([ `Final of string
  | `Initial of string
  | `Location of string * (Command.t * string) list
  | `Variables of string list ]))

let _menhir_action_07 =
  fun _1 ->
    (
# 76 "lib/AutParser.mly"
                        ( `Initial _1 )
# 256 "lib/AutParser.ml"
     : ([ `Final of string
  | `Initial of string
  | `Location of string * (Command.t * string) list
  | `Variables of string list ]))

let _menhir_action_08 =
  fun _1 ->
    (
# 77 "lib/AutParser.mly"
                        ( `Final _1 )
# 267 "lib/AutParser.ml"
     : ([ `Final of string
  | `Initial of string
  | `Location of string * (Command.t * string) list
  | `Variables of string list ]))

let _menhir_action_09 =
  fun _1 ->
    (
# 78 "lib/AutParser.mly"
                        ( `Location _1 )
# 278 "lib/AutParser.ml"
     : ([ `Final of string
  | `Initial of string
  | `Location of string * (Command.t * string) list
  | `Variables of string list ]))

let _menhir_action_10 =
  fun _1 ->
    (
# 128 "lib/AutParser.mly"
                        ( Command.Expression.Cst _1 )
# 289 "lib/AutParser.ml"
     : (Command.Expression.t))

let _menhir_action_11 =
  fun _1 ->
    (
# 129 "lib/AutParser.mly"
                        ( Command.Expression.Var _1 )
# 297 "lib/AutParser.ml"
     : (Command.Expression.t))

let _menhir_action_12 =
  fun _2 ->
    (
# 131 "lib/AutParser.mly"
  ( _2 )
# 305 "lib/AutParser.ml"
     : (Command.Expression.t))

let _menhir_action_13 =
  fun e f ->
    let o = 
# 144 "lib/AutParser.mly"
                        ( Command.Expression.Add )
# 313 "lib/AutParser.ml"
     in
    (
# 133 "lib/AutParser.mly"
  ( Command.Expression.Op (e, o, f) )
# 318 "lib/AutParser.ml"
     : (Command.Expression.t))

let _menhir_action_14 =
  fun e f ->
    let o = 
# 145 "lib/AutParser.mly"
                        ( Command.Expression.Sub )
# 326 "lib/AutParser.ml"
     in
    (
# 133 "lib/AutParser.mly"
  ( Command.Expression.Op (e, o, f) )
# 331 "lib/AutParser.ml"
     : (Command.Expression.t))

let _menhir_action_15 =
  fun e f ->
    let o = 
# 146 "lib/AutParser.mly"
                        ( Command.Expression.Mul )
# 339 "lib/AutParser.ml"
     in
    (
# 133 "lib/AutParser.mly"
  ( Command.Expression.Op (e, o, f) )
# 344 "lib/AutParser.ml"
     : (Command.Expression.t))

let _menhir_action_16 =
  fun e f ->
    let o = 
# 147 "lib/AutParser.mly"
                        ( Command.Expression.Div )
# 352 "lib/AutParser.ml"
     in
    (
# 133 "lib/AutParser.mly"
  ( Command.Expression.Op (e, o, f) )
# 357 "lib/AutParser.ml"
     : (Command.Expression.t))

let _menhir_action_17 =
  fun _2 ->
    (
# 135 "lib/AutParser.mly"
  ( match _2 with
    | Command.Expression.Cst c ->
       Command.Expression.Cst (- c)
    | Command.Expression.Var _
    | Command.Expression.Op (_, _, _) as e ->
       Command.Expression.Op
         (Command.Expression.Cst 0, Command.Expression.Sub, e) )
# 371 "lib/AutParser.ml"
     : (Command.Expression.t))

let _menhir_action_18 =
  fun _2 ->
    (
# 93 "lib/AutParser.mly"
  ( _2 )
# 379 "lib/AutParser.ml"
     : (string))

let _menhir_action_19 =
  fun _2 _3 ->
    (
# 97 "lib/AutParser.mly"
  ( (_2, _3) )
# 387 "lib/AutParser.ml"
     : (string * (Command.t * string) list))

let _menhir_action_20 =
  fun _1 _2 _3 ->
    (
# 114 "lib/AutParser.mly"
  ( Command.Guard (_1, _2, _3) )
# 395 "lib/AutParser.ml"
     : (Command.t))

let _menhir_action_21 =
  fun _2 ->
    (
# 89 "lib/AutParser.mly"
  ( _2 )
# 403 "lib/AutParser.ml"
     : (string))

let _menhir_action_22 =
  fun () ->
    (
# 208 "<standard.mly>"
    ( [] )
# 411 "lib/AutParser.ml"
     : (string list))

let _menhir_action_23 =
  fun x xs ->
    (
# 210 "<standard.mly>"
    ( x :: xs )
# 419 "lib/AutParser.ml"
     : (string list))

let _menhir_action_24 =
  fun () ->
    (
# 208 "<standard.mly>"
    ( [] )
# 427 "lib/AutParser.ml"
     : ([ `Final of string
  | `Initial of string
  | `Location of string * (Command.t * string) list
  | `Variables of string list ] list))

let _menhir_action_25 =
  fun x xs ->
    (
# 210 "<standard.mly>"
    ( x :: xs )
# 438 "lib/AutParser.ml"
     : ([ `Final of string
  | `Initial of string
  | `Location of string * (Command.t * string) list
  | `Variables of string list ] list))

let _menhir_action_26 =
  fun () ->
    (
# 208 "<standard.mly>"
    ( [] )
# 449 "lib/AutParser.ml"
     : ((Command.t * string) list))

let _menhir_action_27 =
  fun x xs ->
    (
# 210 "<standard.mly>"
    ( x :: xs )
# 457 "lib/AutParser.ml"
     : ((Command.t * string) list))

let _menhir_action_28 =
  fun _1 _3 ->
    (
# 72 "lib/AutParser.mly"
  ( output _1 _3 )
# 465 "lib/AutParser.ml"
     : (string * string list * string option * string option *
  (string * (Command.t * string) list) list))

let _menhir_action_29 =
  fun () ->
    (
# 120 "lib/AutParser.mly"
                        ( Command.Predicate.Eq )
# 474 "lib/AutParser.ml"
     : (Command.Predicate.op))

let _menhir_action_30 =
  fun () ->
    (
# 121 "lib/AutParser.mly"
                        ( Command.Predicate.Lst )
# 482 "lib/AutParser.ml"
     : (Command.Predicate.op))

let _menhir_action_31 =
  fun () ->
    (
# 122 "lib/AutParser.mly"
                        ( Command.Predicate.Gst )
# 490 "lib/AutParser.ml"
     : (Command.Predicate.op))

let _menhir_action_32 =
  fun () ->
    (
# 123 "lib/AutParser.mly"
                        ( Command.Predicate.Leq )
# 498 "lib/AutParser.ml"
     : (Command.Predicate.op))

let _menhir_action_33 =
  fun () ->
    (
# 124 "lib/AutParser.mly"
                        ( Command.Predicate.Geq )
# 506 "lib/AutParser.ml"
     : (Command.Predicate.op))

let _menhir_action_34 =
  fun () ->
    (
# 125 "lib/AutParser.mly"
                        ( Command.Predicate.Neq )
# 514 "lib/AutParser.ml"
     : (Command.Predicate.op))

let _menhir_action_35 =
  fun () ->
    (
# 117 "lib/AutParser.mly"
                        ( Command.Skip )
# 522 "lib/AutParser.ml"
     : (Command.t))

let _menhir_action_36 =
  fun _2 _4 ->
    (
# 101 "lib/AutParser.mly"
  ( (_2, _4) )
# 530 "lib/AutParser.ml"
     : (Command.t * string))

let _menhir_action_37 =
  fun _2 _3 ->
    (
# 82 "lib/AutParser.mly"
  ( _2 :: _3 )
# 538 "lib/AutParser.ml"
     : (string list))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | TK_ADD ->
        "TK_ADD"
    | TK_ARROW ->
        "TK_ARROW"
    | TK_ASSIGN ->
        "TK_ASSIGN"
    | TK_CHOICE ->
        "TK_CHOICE"
    | TK_COMMA ->
        "TK_COMMA"
    | TK_DIV ->
        "TK_DIV"
    | TK_EOF ->
        "TK_EOF"
    | TK_EQ ->
        "TK_EQ"
    | TK_FINAL ->
        "TK_FINAL"
    | TK_FROM ->
        "TK_FROM"
    | TK_GEQ ->
        "TK_GEQ"
    | TK_GST ->
        "TK_GST"
    | TK_INITIAL ->
        "TK_INITIAL"
    | TK_LBRACE ->
        "TK_LBRACE"
    | TK_LEQ ->
        "TK_LEQ"
    | TK_LPAREN ->
        "TK_LPAREN"
    | TK_LST ->
        "TK_LST"
    | TK_MUL ->
        "TK_MUL"
    | TK_NAT _ ->
        "TK_NAT"
    | TK_NEQ ->
        "TK_NEQ"
    | TK_RBRACE ->
        "TK_RBRACE"
    | TK_RPAREN ->
        "TK_RPAREN"
    | TK_SEMICOLON ->
        "TK_SEMICOLON"
    | TK_SKIP ->
        "TK_SKIP"
    | TK_STR _ ->
        "TK_STR"
    | TK_SUB ->
        "TK_SUB"
    | TK_VAR ->
        "TK_VAR"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_59 : type  ttv_stack. ttv_stack _menhir_cell0_TK_STR -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TK_EOF ->
          let MenhirCell0_TK_STR (_menhir_stack, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_28 _1 _3 in
          MenhirBox_main _v
      | _ ->
          _eRR ()
  
  let rec _menhir_run_66 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_declaration -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_declaration (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_25 x xs in
      _menhir_goto_list_declaration_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_list_declaration_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState65 ->
          _menhir_run_66 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState02 ->
          _menhir_run_59 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  let rec _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_TK_VAR (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TK_STR _v ->
          let _menhir_stack = MenhirCell0_TK_STR (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TK_COMMA ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState04
          | TK_SEMICOLON ->
              let _v_0 = _menhir_action_22 () in
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_05 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TK_STR _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _2 = _v in
          let _v = _menhir_action_02 _2 in
          let _menhir_stack = MenhirCell1_comma_string (_menhir_stack, _menhir_s, _v) in
          (match (_tok : MenhirBasics.token) with
          | TK_COMMA ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState09
          | TK_SEMICOLON ->
              let _v_0 = _menhir_action_22 () in
              _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_10 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_comma_string -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_comma_string (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_23 x xs in
      _menhir_goto_list_comma_string_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_list_comma_string_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState09 ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState04 ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_07 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_TK_VAR _menhir_cell0_TK_STR -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell0_TK_STR (_menhir_stack, _2) = _menhir_stack in
      let MenhirCell1_TK_VAR (_menhir_stack, _menhir_s) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_37 _2 _3 in
      let _1 = _v in
      let _v = _menhir_action_06 _1 in
      _menhir_goto_declaration _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_declaration : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_declaration (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TK_VAR ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState65
      | TK_INITIAL ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState65
      | TK_FROM ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState65
      | TK_FINAL ->
          _menhir_run_55 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState65
      | TK_RBRACE ->
          let _v_0 = _menhir_action_24 () in
          _menhir_run_66 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | _ ->
          _eRR ()
  
  and _menhir_run_11 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TK_STR _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TK_SEMICOLON ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _2 = _v in
              let _v = _menhir_action_21 _2 in
              let _1 = _v in
              let _v = _menhir_action_07 _1 in
              _menhir_goto_declaration _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_14 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_TK_FROM (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TK_STR _v ->
          let _menhir_stack = MenhirCell0_TK_STR (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TK_CHOICE ->
              _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
          | TK_FINAL | TK_FROM | TK_INITIAL | TK_RBRACE | TK_VAR ->
              let _v_0 = _menhir_action_26 () in
              _menhir_run_54 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_16 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_TK_CHOICE (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TK_SUB ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState16
      | TK_STR _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TK_ASSIGN ->
              let _menhir_stack = MenhirCell1_TK_STR (_menhir_stack, MenhirState16, _v) in
              let _menhir_s = MenhirState33 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TK_SUB ->
                  _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | TK_STR _v ->
                  _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | TK_NAT _v ->
                  _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | TK_LPAREN ->
                  _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | TK_ADD | TK_DIV | TK_EQ | TK_GEQ | TK_GST | TK_LEQ | TK_LST | TK_MUL | TK_NEQ | TK_SUB ->
              let _v =
                let _1 = _v in
                _menhir_action_11 _1
              in
              _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState16 _tok
          | _ ->
              _eRR ())
      | TK_SKIP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_35 () in
          let _1 = _v in
          let _v = _menhir_action_05 _1 in
          _menhir_goto_command _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TK_NAT _v ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState16
      | TK_LPAREN ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState16
      | _ ->
          _eRR ()
  
  and _menhir_run_17 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_TK_SUB (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState17 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TK_SUB ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TK_STR _v ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TK_NAT _v ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TK_LPAREN ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_18 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_11 _1 in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState45 ->
          _menhir_run_46 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState16 ->
          _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState33 ->
          _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState17 ->
          _menhir_run_31 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState29 ->
          _menhir_run_30 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState26 ->
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState24 ->
          _menhir_run_25 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState22 ->
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState20 ->
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_46 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_TK_CHOICE, _menhir_box_main) _menhir_cell1_expression _menhir_cell0_relop as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TK_SUB ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TK_MUL ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TK_DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TK_ADD ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TK_ARROW ->
          let MenhirCell0_relop (_menhir_stack, _2) = _menhir_stack in
          let MenhirCell1_expression (_menhir_stack, _, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_20 _1 _2 _3 in
          let _1 = _v in
          let _v = _menhir_action_04 _1 in
          _menhir_goto_command _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_22 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState22 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TK_SUB ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TK_STR _v ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TK_NAT _v ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TK_LPAREN ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_19 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_10 _1 in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_20 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_TK_LPAREN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState20 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TK_SUB ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TK_STR _v ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TK_NAT _v ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TK_LPAREN ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_24 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState24 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TK_SUB ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TK_STR _v ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TK_NAT _v ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TK_LPAREN ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_26 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState26 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TK_SUB ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TK_STR _v ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TK_NAT _v ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TK_LPAREN ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_29 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState29 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TK_SUB ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TK_STR _v ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TK_NAT _v ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TK_LPAREN ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_command : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_TK_CHOICE -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | TK_ARROW ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TK_STR _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TK_SEMICOLON ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let MenhirCell1_TK_CHOICE (_menhir_stack, _menhir_s) = _menhir_stack in
                  let (_2, _4) = (_v, _v_0) in
                  let _v = _menhir_action_36 _2 _4 in
                  let _menhir_stack = MenhirCell1_transition (_menhir_stack, _menhir_s, _v) in
                  (match (_tok : MenhirBasics.token) with
                  | TK_CHOICE ->
                      _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState52
                  | TK_FINAL | TK_FROM | TK_INITIAL | TK_RBRACE | TK_VAR ->
                      let _v_0 = _menhir_action_26 () in
                      _menhir_run_53 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 _tok
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_53 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_transition -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_transition (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_27 x xs in
      _menhir_goto_list_transition_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_list_transition_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState15 ->
          _menhir_run_54 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState52 ->
          _menhir_run_53 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_54 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_TK_FROM _menhir_cell0_TK_STR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_TK_STR (_menhir_stack, _2) = _menhir_stack in
      let MenhirCell1_TK_FROM (_menhir_stack, _menhir_s) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_19 _2 _3 in
      let _1 = _v in
      let _v = _menhir_action_09 _1 in
      _menhir_goto_declaration _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_38 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_TK_CHOICE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TK_SUB ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TK_NEQ ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_34 () in
          _menhir_goto_relop _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TK_MUL ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TK_LST ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_30 () in
          _menhir_goto_relop _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TK_LEQ ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_32 () in
          _menhir_goto_relop _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TK_GST ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_31 () in
          _menhir_goto_relop _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TK_GEQ ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_33 () in
          _menhir_goto_relop _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TK_EQ ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_29 () in
          _menhir_goto_relop _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TK_DIV ->
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TK_ADD ->
          _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_goto_relop : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_TK_CHOICE, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_relop (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | TK_SUB ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState45
      | TK_STR _v_0 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState45
      | TK_NAT _v_1 ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState45
      | TK_LPAREN ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState45
      | _ ->
          _eRR ()
  
  and _menhir_run_34 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_TK_CHOICE, _menhir_box_main) _menhir_cell1_TK_STR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TK_SUB ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TK_MUL ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TK_DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TK_ADD ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TK_ARROW ->
          let MenhirCell1_TK_STR (_menhir_stack, _, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_01 _1 _3 in
          let _1 = _v in
          let _v = _menhir_action_03 _1 in
          _menhir_goto_command _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_31 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_TK_SUB -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_TK_SUB (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_17 _2 in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_30 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TK_MUL ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TK_DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TK_ADD | TK_ARROW | TK_EQ | TK_GEQ | TK_GST | TK_LEQ | TK_LST | TK_NEQ | TK_RPAREN | TK_SUB ->
          let MenhirCell1_expression (_menhir_stack, _menhir_s, e) = _menhir_stack in
          let f = _v in
          let _v = _menhir_action_13 e f in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_27 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expression (_menhir_stack, _menhir_s, e) = _menhir_stack in
      let f = _v in
      let _v = _menhir_action_16 e f in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_25 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expression (_menhir_stack, _menhir_s, e) = _menhir_stack in
      let f = _v in
      let _v = _menhir_action_15 e f in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_23 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TK_MUL ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TK_DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TK_ADD | TK_ARROW | TK_EQ | TK_GEQ | TK_GST | TK_LEQ | TK_LST | TK_NEQ | TK_RPAREN | TK_SUB ->
          let MenhirCell1_expression (_menhir_stack, _menhir_s, e) = _menhir_stack in
          let f = _v in
          let _v = _menhir_action_14 e f in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_21 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_TK_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TK_SUB ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TK_RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_TK_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_12 _2 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | TK_MUL ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TK_DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TK_ADD ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_55 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TK_STR _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TK_SEMICOLON ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _2 = _v in
              let _v = _menhir_action_18 _2 in
              let _1 = _v in
              let _v = _menhir_action_08 _1 in
              _menhir_goto_declaration _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TK_STR _v ->
          let _menhir_stack = MenhirCell0_TK_STR (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TK_LBRACE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TK_VAR ->
                  _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState02
              | TK_INITIAL ->
                  _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState02
              | TK_FROM ->
                  _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState02
              | TK_FINAL ->
                  _menhir_run_55 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState02
              | TK_RBRACE ->
                  let _v_0 = _menhir_action_24 () in
                  _menhir_run_59 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
end

let main =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_main v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
