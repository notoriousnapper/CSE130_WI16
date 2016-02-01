(* CSE 130: Programming Assignment 3
 * misc.ml
 *)

(* For this assignment, you may use the following library functions:

   List.map
   List.fold_left
   List.fold_right
   List.split
   List.combine
   List.length
   List.append
   List.rev

   See http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html for
   documentation.
*)



(* Do not change the skeleton code! The point of this assignment is to figure
 * out how the functions can be written this way (using fold). You may only
 * replace the   failwith "to be implemented"   part. *)



(*****************************************************************)
(******************* 1. Warm Up   ********************************)
(*****************************************************************)

let sqsum xs = 
  let f a x = a + (x * x) in  
  (* Function to be applied over List.fold_left *)
  (* x is head element of list *)
  let base = 0 in   
  (* base = acc*)
    List.fold_left f base xs
  (* Testing *)

  (* let test = fun a x -> a + (x * x)
  List.fold_left test 0 [3;4;5;6];;
  *)



(* Basically, takes a list of functions, and arranges them such 
   that each preceding function is an argument for the next function.
   ex. fn(...(f2(f1 x))).

   Note: pipe fs returns a FUNCTION, which you use on a SECOND parameter.
   Hence, the call is ((pipe [function list])) (int)
*)

(* my mistake was let f a x = (x a) in, it should be a function that
   returns a function!!! but --> that just solves the function.
   subtle difference --> specifically, it is
   a function that takes in an int, and returns the stacked function
   f2( f1 (b)) *)

let pipe fs = 
  let f a x = fun b -> x (a b) in
  let base = (fun x -> x) in
    List.fold_left f base fs

(* Takes in a separator string, and strings together 
   strings in a list, with the custom separator delimiting each string
 *)

(* Sep Concat works like this too...wtf *)
let sepC sep l = 
  let f a x = a ^ sep ^ x in
  let base = "" in 
	List.fold_left f base l
(* However, need a case for if its the very first element, which is
why below option exists *)

(* ran into error Error: This expression has type string -> string -> string
       but an expression was expected of type
         ('a -> 'b -> 'c -> 'd) -> 'e -> 'a -> 'b -> 'c -> 'd
       Type string is not compatible with type 'a -> 'b -> 'c
*)
let rec sepConcat sep sl = match sl with 
  | [] -> ""
  | h :: t -> 
      let f a x = a ^ sep ^ x in 
      let base = h in 
      let l = t in 
        List.fold_left f base l



(* Ran into error: 
Error: This expression has type 'a list -> 'b list
       but an expression was expected of type string list
	Apparently when list for sepConcat is (List.map (f l)) 
        it doesn't work..? 
*)
let stringOfList f l = "["^ (sepConcat "; " (List.map f l)) ^ "]"

(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)

let rec clone x n = 
    let acc = [] in 
    if (n <= 0) then acc
    else 
        x :: clone x (n-1)
            
let rec padZero l1 l2 = 
    let len1 = if l1 = [] then 0 else List.length l1 in 
        let len2 = if l2 = [] then 0 else List.length l2 in 
	    let diff = len1 - len2 in 
		if(diff == 0) then (l1, l2)
		    else if (diff > 0) then 
			let newl2 = List.append [0] l2 in
			padZero l1 newl2
		    else 
			let newl1 = List.append [0] l1 in
			padZero newl1 l2
(* For Testing *)
(*
open Printf
let testList =  (List.append [0] [2;3])
(*let _= List.iter (print_int) testList;; *)

let tupleCombine (x,y) = 
	List.append x y;;


let _= List.iter (print_int) (tupleCombine (padZero [0][2;3]));;
let _= print_string "\n";;
let _= List.iter (print_int) (tupleCombine (padZero [2;3][1]));;
let _= print_string "\n";;
let _= List.iter (print_int) (tupleCombine (padZero [][2;3]));;
let _= print_string "\n";;
let _= List.iter (print_int) (tupleCombine (padZero [2;3][]));;
let _= print_string "\n";;
let _= print_int (List.length []);;
let _= print_string "\n";;
let _= print_int (List.length [3;2]);;
let _= print_string "\n";;

padZero [9;9] [1;0;0;2];;
(*- : int list * int list = ([0;0;9;9],[1;0;0;2]) *)
padZero [1;0;0;2] [9;9];;
(*- : int list * int list = ([1;0;0;2],[0;0;9;9]) *)
*)


let rec removeZero l = match l with 
    | [] -> []
    | h::t -> if h == 0 then removeZero t 
                  else h::t 

    (* Note that base should match what args is,because base = base case *)
    (* Number on left is remainder *)
    (* Trick is to have a list of tupled ints *,
     because of the way fold_left works *)
let bigAdd l1 l2 = 
  let add (l1, l2) = 
    let f a x = 
	let (h, h') = x in 
	    let newDigit = h + h'  in 
		let (rem, acc) = a in 
		let newRem = if (newDigit + rem) >= 10 then 1 else 0 
		in 
		let newNewDigit = (newDigit + rem) mod 10 in 
		let maxLen = if (List.length l1 > List.length l2) 
		    then List.length l1
		    else List.length l2 in 
		if (List.length acc ==  (maxLen-1)) then 
		     (0, newRem::newNewDigit::acc) 
		else 
		     (newRem, newNewDigit::acc)
             (*(0, (h+h')::a)*)
	in
    let base = (0,[]) in    
    let args = List.rev (List.combine l1 l2) in  
    let (_, res) = List.fold_left f base args in
      res
  in 
    removeZero (add (padZero l1 l2));;

let x = [9;9;9;9];;
let y = [9;9;9];;
let z = [1;0;9;9;8];;

let _= List.iter (print_int) (bigAdd [1;2][1;2]);;
let _= List.iter (print_int) (bigAdd x y );; 

(* fold_left implementation: 
  let rec fold_left f a l = 
	match l with 
	[] -> a
	| h::t -> fold_left f (f a h) t 
*)

let rec mulByDigit i l = 
  let f a h = (i * h)::a in 
  match l with 
    | [] -> a 
    | h::t -> mulByDigit (f a h) t 


let bigMul l1 l2 = 
  let f a x = failwith "to be implemented" in
  let base = failwith "to be implemented" in
  let args = failwith "to be implemented" in
  let (_, res) = List.fold_left f base args in
    res
