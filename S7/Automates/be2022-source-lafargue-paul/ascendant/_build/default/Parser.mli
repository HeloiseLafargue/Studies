
(* The type of tokens. *)

type token = 
  | UL_PT
  | UL_PAROUV
  | UL_PARFER
  | UL_IDENT of (string)
  | UL_FIN
  | UL_ENTIER of (int)

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val scheme: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (unit)
