type token =
  | Num of (int)
  | EOF
  | Id of (string)
  | TRUE
  | FALSE
  | LET
  | REC
  | EQ
  | IN
  | FUN
  | ARROW
  | IF
  | THEN
  | ELSE
  | OR
  | AND
  | LT
  | LE
  | NE
  | PLUS
  | MINUS
  | MUL
  | DIV
  | LPAREN
  | RPAREN
  | LBRAC
  | RBRAC
  | SEMI
  | COLONCOLON

val exp :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Nano.expr
