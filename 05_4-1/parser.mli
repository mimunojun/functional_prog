type token =
  | VAR of (string)
  | INT of (int)
  | PLUS
  | MINUS
  | ASTERISK
  | SLASH
  | EQUAL
  | NEQUAL
  | LESS
  | GREATER
  | COLCOL
  | LPAREN
  | RPAREN
  | LBRA
  | RBRA
  | ARROW
  | VBAR
  | SEMICOL
  | TRUE
  | FALSE
  | FUN
  | LET
  | REC
  | IN
  | IF
  | THEN
  | ELSE
  | MATCH
  | WITH
  | HEAD
  | TAIL
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.exp
