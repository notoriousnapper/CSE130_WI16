%{
(*See this for a tutorial on ocamlyacc 
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Nano 
%}

%token <int> Num
%token EOF
%token <string> Id
%token TRUE FALSE

%token LET REC EQ IN
%token FUN ARROW   
%token IF THEN ELSE

%token OR AND
%token LT LE NE   
%token PLUS MINUS MUL DIV
%token LPAREN RPAREN    

%token LBRAC RBRAC     
%token SEMI COLONCOLON

%nonassoc LET FUN IF
%left OR AND
%left EQ LT LE NE
%right COLONCOLON SEMI RBRAC
%left PLUS MINUS MUL DIV
%left APP

%start exp 
%type <Nano.expr> exp

%%

/* Statement based Expressions */
exp: 
    | LET Id EQ exp IN exp      { Let ($2, $4, $6) }
    | LET REC Id EQ exp IN exp  { Letrec ($3, $5, $7) }
    | FUN Id ARROW exp          { Fun ($2, $4) }
    | IF exp THEN exp ELSE exp  { If ($2, $4, $6) }

    | orexp                     {$1}

/* Logical Operations */
orexp: 
    | orexp OR andexp           { Bin ($1, Or, $3) }
    | andexp                    {$1}

andexp: 
    | andexp AND logexp        { Bin ($1, And, $3) }
    | logexp                   {$1}

logexp: 
    | logexp EQ lexp          { Bin ($1, Eq, $3) }
    | logexp NE lexp          { Bin ($1, Ne, $3) }

    | logexp LT lexp          { Bin ($1, Lt, $3) }
    | logexp LE lexp          { Bin ($1, Le, $3) }
    | lexp                      {$1}

lexp: 
    | asexp COLONCOLON lexp     { Bin ($1, Cons, $3) }
    | asexp SEMI lexp           { Bin ($1, Cons, $3) }
    | LBRAC lexp                { $2 }
    | lexp RBRAC                { Bin ($1, Cons, NilExpr) }
    | asexp                     {$1}

/* Arithmetic Expressions */
asexp: 
    | asexp PLUS mdexp          { Bin ($1, Plus, $3) }
    | asexp MINUS mdexp         { Bin ($1, Minus, $3) }
    | mdexp                     {$1}

mdexp: 
    | mdexp DIV fexp            { Bin ($1, Div, $3) }
    | mdexp MUL fexp            { Bin ($1, Mul, $3) }
    | fexp                      {$1}
fexp: 
    | fexp bexp                 { App ($1, $2)}
    | bexp                      {$1}

/* Base Expressions */
bexp: 
    | Num                       { Const $1 }
    | Id                        { Var $1 }
    | TRUE                      { True }
    | FALSE                     { False }
    | LPAREN exp RPAREN         {$2}
    | LBRAC RBRAC               { NilExpr }
