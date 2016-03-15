{
  open Nano        (* nano.ml *)
  open NanoParse   (* nanoParse.ml from nanoParse.mly *)
}

(* This file takes input string coming in from user input or file, 
 * and then parses it into tokens for nanoParse.ml
 * The defined tokens on the right, such as "TRUE, LET, EQ" are defined
 * in nanoParse.ml as well.  If the Tokens are not in both files, will throw
 * an error
 *)

rule token = parse
  | [' ' '\t' '\n' '\r']    { token lexbuf }
  | "true"                  { TRUE }
  | "false"                 { FALSE }

  | "let"                   { LET } 
  | "rec"                   { REC }
  | "="                     { EQ }
  | "in"                    { IN }
  | "fun"                   { FUN }
  | "->"                    { ARROW }

  | "if"                    { IF }
  | "then"                  { THEN }
  | "else"                  { ELSE }

  | "+"                     { PLUS }
  | "-"                     { MINUS }
  | "*"                     { MUL }
  | "/"                     { DIV }
  | "<"                     { LT }
  | "<="                    { LE }
  | "!="                    { NE }
  | "&&"                    { AND }
  | "||"                    { OR }

  | "("                     { LPAREN }
  | ")"                     { RPAREN }

  | "["                     { LBRAC }
  | "]"                     { RBRAC }
  | "::"                    { COLONCOLON }
  | ";"                     { SEMI }

  | ['0'-'9']+ as inum      { Num(int_of_string inum) }
  | ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9']* as str { Id(str) }
  | eof                     { EOF }
  | _                       { raise (MLFailure ("Illegal Character '"^(Lexing.lexeme lexbuf)^"'")) }
