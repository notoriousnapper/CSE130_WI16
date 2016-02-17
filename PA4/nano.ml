exception MLFailure of string

type binop = 
  Plus 
| Minus 
| Mul 
| Div 
| Eq 
| Ne 
| Lt 
| Le 
| And 
| Or          
| Cons

type expr =   
  Const of int 
| True   
| False      
| NilExpr
| Var of string    
| Bin of expr * binop * expr 
| If  of expr * expr * expr
| Let of string * expr * expr 
| App of expr * expr 
| Fun of string * expr    
| Letrec of string * expr * expr
	
type value =  
  Int of int		
| Bool of bool          
| Closure of env * string option * string * expr 
| Nil                    
| Pair of value * value     

and env = (string * value) list

let binopToString op = 
  match op with
      Plus -> "+" 
    | Minus -> "-" 
    | Mul -> "*" 
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v = 
  match v with 
    Int i -> 
      Printf.sprintf "%d" i
  | Bool b -> 
      Printf.sprintf "%b" b
  | Closure (evn,fo,x,e) -> 
      let fs = match fo with None -> "Anon" | Some fs -> fs in
      Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
  | Pair (v1,v2) -> 
      Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2) 
  | Nil -> 
      "[]"

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
  "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | True -> 
        "true" 
    | False -> 
        "false"
    | Var x -> 
        x
    | Bin (e1,op,e2) -> 
        Printf.sprintf "%s %s %s" 
        (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) -> 
        Printf.sprintf "if %s then %s else %s" 
        (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "let %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 
    | App (e1,e2) -> 
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) -> 
        Printf.sprintf "fun %s -> %s" x (exprToString e) 
    | Letrec (x,e1,e2) -> 
        Printf.sprintf "let rec %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 

(*********************** Some helpers you might need ***********************)

let rec fold f base args = 
  match args with [] -> base
    | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) = 
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)


(* Function that deals with lookup for a variable in an environment
 *)

let lookup (x,evn) = 
    match listAssoc(x,evn) with 
    | Some x -> x
    | _-> raise (MLFailure ("Variable not bound: " ^ x))



(* Function mirroring how ocaml statements are evaluated.
 * First block deals with singular, uncomplicated expressions 
 * Second Block deals with binary operations, including arithmetic, logical op, and cons
 * Third Block deals with If statement, evaluating each segment
 *)

let rec eval (evn,e) = match e with
    | Const c -> Int c
    | True -> Bool true
    | False -> Bool false
    | NilExpr -> Nil
    | Var s -> lookup (s, evn)


    | Bin (le,op,re) -> (
        let l = eval (evn, le) in
        let r = eval (evn, re) in
        match (l,op,r) with
        | Int l, Plus, Int r -> Int (l + r)
        | Int l, Minus, Int r -> Int (l - r)
        | Int l, Mul, Int r -> Int (l * r)
        | Int l, Div, Int r -> Int (l / r)
        | Int l, Eq, Int r -> Bool (l = r)
        | Int l, Ne, Int r -> Bool (l != r)
        | Int l, Lt, Int r -> Bool (l < r)
        | Int l, Le, Int r -> Bool (l <= r)
        | Bool l, Eq, Bool r -> Bool (l = r)
        | Bool l, Ne, Bool r -> Bool (l != r)
        | Bool l, And, Bool r -> Bool (l && r)
        | Bool l, Or, Bool r -> Bool (l || r)
        | Int l, Cons, Nil -> Pair (Int l, Nil)
        | Int l, Cons, Pair(a,b) -> Pair (Int l, r)
        | _ -> raise (MLFailure ("Unsupported operation: " ^ exprToString e))
      )
    |If (p,t,f) -> (
    	match eval (evn, p) with
        | Bool b -> if b then eval (evn,t) else eval (evn,f)
        | _ -> raise (MLFailure ("Expression (" ^ exprToString p ^ ") does not evaluate to a Bool"))
  	)
    | Let (b,le,re) -> (
        let evn = (b, eval (evn,le))::evn in eval (evn,re)
        )

    | Letrec (b,le,re) -> (
        let v = eval (evn, le) in
        let evn1 = (
        match v with
        | Closure (evn',None,x,e) -> Closure (evn',Some b,x,e)
        | _ -> v
        ) in
            let evn2 = (b,evn1)::evn in eval (evn2,re)
        )
  | App (le,re) -> (
    let Closure (evn1,f,x,e) = eval (evn,le) in
    let v = eval (evn, re) in
    let evn' = (
      match f with
      | Some n -> (n, Closure (evn1,f,x,e))::(x,v)::evn1
      | None -> (x,v)::evn1
    ) in eval (evn', e)
  )
  | Fun (x,e) -> Closure (evn, None, x, e)



          

(**********************     Testing Code  ******************************)
