
(* The type of tokens. *)

type token = 
  | TK_VAR
  | TK_SUB
  | TK_STR of (string)
  | TK_SKIP
  | TK_SEMICOLON
  | TK_RPAREN
  | TK_RBRACE
  | TK_NEQ
  | TK_NAT of (int)
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

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (string * string list * string option * string option *
  (string * (Command.t * string) list) list)
