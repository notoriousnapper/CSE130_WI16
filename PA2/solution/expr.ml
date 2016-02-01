(*
 * expr.ml
 * cse130
 * based on code by Chris Stone
 * By: Jesse Ren A10650321
 *)

(* Please do not modify the names or types of any of the following
 * type constructors, or predefined functions, unless EXPLICITLY
 * asked to. You will loose points if you do.
 *)


(* REMEMBER TO DOCUMENT ALL FUNCTIONS THAT YOU WRITE OR COMPLETE *)

type expr = 
    VarX
  | VarY
  | Sine     of expr
  | Cosine   of expr
  | Average  of expr * expr
  | Times    of expr * expr
  | Thresh   of expr * expr * expr * expr
  | Squared of expr 
  | TimesThree of expr * expr * expr

(*
    function: exprToString
    Input:   expr -> string
    Big Idea: Prints Expressions
    Algorithm: More of Just using string concatenation to print strings 
	       correctly
*)

let rec exprToString e = 
  match e with
  | VarX -> "x"
  | VarY -> "y"
  | Sine e'-> "sin(pi*" ^ exprToString e' ^ ")"
  | Cosine e'-> "cos(pi*" ^ exprToString e' ^ ")"
  | Average (a,b) -> "((" ^ exprToString a ^ "+" ^ exprToString b ^ ")/2)"
  | Times (a,b) -> exprToString a ^ "*" ^ exprToString b
  | Thresh (a,b,c,d) -> "(" ^ exprToString a ^ "<" ^ exprToString b ^ "?" ^ exprToString c ^ ":" ^ exprToString d ^ ")"
  | Squared e' -> exprToString e' ^ "*" ^ exprToString e' 
  | TimesThree (a,b,c) -> "(" ^ exprToString a ^ "*" ^ exprToString b ^ "*" ^ exprToString c ^ ")"
;;

(*
    function: build__()
    Input:  Varies 
    Big Idea: Helper Functions that are better than just contructors 
*)

let buildX()                       = VarX
let buildY()                       = VarY
let buildSine(e)                   = Sine(e)
let buildCosine(e)                 = Cosine(e)
let buildAverage(e1,e2)            = Average(e1,e2)
let buildTimes(e1,e2)              = Times(e1,e2)
let buildThresh(a,b,a_less,b_less) = Thresh(a,b,a_less,b_less)
let buildSquared(e)                  = Squared(e)
let buildTimesThree(e1,e2,e3)        = TimesThree(e1,e2,e3)


let pi = 4.0 *. atan 1.0

(* functions: eval 
     Input: expr * float * float -> float
     BigIdea:  Evaluates a given expression
	Calls upon mathematical operations
*)
let rec eval (e,x,y) = 
  match e with
  | VarX -> x
  | VarY -> y
  | Sine (e') -> sin(pi *. (eval (e',x,y)) )
  | Cosine (e') -> cos(pi *. (eval (e',x,y)) )
  | Average (a,b) -> (eval (a,x,y) +. eval (b,x,y)) /. 2.0
  | Times (a,b) -> eval (a,x,y) *. eval (b,x,y)
  | Thresh (a,b,c,d) -> if ((eval (a,x,y) ) < (eval (b,x,y))) then eval (c,x,y) else eval (d,x,y)
  | Squared (e') -> (-1.0) *. eval (e',x,y)
  | TimesThree (a,b,c) -> (eval (a,x,y) *. eval (b,x,y) *. eval (c,x,y)) 
;;

let eval_fn e (x,y) = 
  let rv = eval (e,x,y) in
  assert (-1.0 <= rv && rv <= 1.0);
  rv

let sampleExpr =
      buildCosine(buildSine(buildTimes(buildCosine(buildAverage(buildCosine(
      buildX()),buildTimes(buildCosine (buildCosine (buildAverage
      (buildTimes (buildY(),buildY()),buildCosine (buildX())))),
      buildCosine (buildTimes (buildSine (buildCosine
      (buildY())),buildAverage (buildSine (buildX()), buildTimes
      (buildX(),buildX()))))))),buildY())))

let sampleExpr2 =
  buildThresh(buildX(),buildY(),buildSine(buildX()),buildCosine(buildY()))


(************** Add Testing Code Here ***************)
