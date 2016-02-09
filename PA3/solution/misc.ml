(* CSE 130: Programming Assignment 3
 * misc.ml
 * Author: Jesse Ren 
 * Late Days Used: 1 
 * Date: 2/2/16
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









(*
 * Name: pipe
 * Type: (a -> b) List -> (a -> b) 
 * Function: This function takes in a list of functions, and returns a single function 
 * that calls each subsequent function on the result of the previous function.  Calling 
 * the function resulting from pipe will result in all functions being run in a russian doll
 * like fashion for a single input.
 * Implementation: Utilizes fold_left, with base as a function that simply returns an input, and the 
 *   helper function being a function that takes in a function that takes in an input, and returns f2(f1(x))
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

(*
 * Name: sepConcat
 * Type: String -> String list -> String
 * Function: This function takes in a delimiting string for separating, and a list of strings, 
 * and returns a string of all the strings concatenated with the separator between each original 
 * element.
 * Implementation: Simple concatenation using fold_left and accumulating a new element and a separator.
 * The very first element of the String list, however, does not have a separator.
 *)
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



(*
 * Name: clone
 * Type: int -> a' list 
 * Function: clones an element n times, depending on the 1st parameter.  Zero for n, or an empty element, 
 * gives an empty list.
 * Implementation: Recursively feed back the same parameters, except first one, "n," is returned as "n-1"
 * in each itereation
 *)




let rec clone x n = 
    let acc = [] in 
    if (n <= 0) then acc
    else 
        x :: clone x (n-1)

(*
 * Name: padZero
 * Type: int List -> int List -> (int List , int List)
 * Function: Takes two integer lists, and returns a tuple with the two lists, except
 * the lists will be padded with 0's in front to have the same length
 * Implementation:
 *)

            
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
(*
 * Name: removeZero 
 * Type: int List -> int List 
 * Function: Strips away the zeroes in front of a list of ints.
 * Implementation: Recursively check if head is a 0, and if so, return the tail to itself, until the
 * head of the list is non-zero. 
 *)


let rec removeZero l = match l with 
    | [] -> []
    | h::t -> if h == 0 then removeZero t 
                  else h::t 

(* Name: bigAdd
 * Type: int List -> int List -> int List
 * Function:  Adds two big integers represented as a list of ints for each digit
 * Implementation: Adds in each level, the integers, while keeping track of 
 * a remainder for each
 * recursion to the next step if needed.
 *)











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

(* Testing 
let x = [9;9;9;9];;
let y = [9;9;9];;
let z = [1;0;9;9;8];;

let _= List.iter (print_int) (bigAdd [1;2][1;2]);;
let _= List.iter (print_int) (bigAdd x y );; 
*)

(* fold_left implementation: 
  let rec fold_left f a l = 
	match l with 
	[] -> a
	| h::t -> fold_left f (f a h) t 
*)



(* Helper function from Pa1 *)
let rec digitsOfInt n = match n with
| 0 -> []
| x ->
digitsOfInt (n / 10)@[(n mod 10)]





(*
 * Name: mulByDigit
 * Type: int -> int List -> int [List]
 * Function: Multiplies a number (represented by a list, with each element as a digit), by an int
 * Implementation: Uses digitsofInt from PA1 to change products with more than 1 digit to a list.
 * This function starts from the head, and pads zeroes based on the length of the remaining list (parsed
 * by one element.  Ex: in 3 * [3;2;1], it, in first level, will add 
 * [3*3]::[length-1 # of Zeroes], which is [900], to the accumulator, which is [0] in the beginning
 *)


let rec mulByDigit i l = 
  match l with 
  | [] -> [0] 
  | h::t -> 
	let prod = h * i in 
        let padProd = 
        if ((prod)>9) then digitsOfInt prod else [prod] in 
        let param1 = 
	List.append (padProd) (clone 0 (List.length t)) in 
        let param2 = (mulByDigit i t) in 
        bigAdd (param1) (param2)

let x = [9;9;9;9];;
let y = 9;;
(*
let _= print_string "\nJesse\n";;
let _= List.iter print_int (mulByDigit 9 [9]);;
let _= print_string "\nRen\n";;
let _= List.iter print_int (mulByDigit 9 x);;
let _= print_string "\nRocks\n";;
let _= List.iter print_int (mulByDigit 1 x);;
*)


(*
  let f a h = (i * h)::a in 
  match l with 
    | [] -> []
    | [ht,ht'] -> ht'
    | h::t -> (mulByDigit i t) ::
*)



(*
 * Name: bigMul
 * Type: int List -> int List -> int List 
 * Function: Multiplies two large numbers, which are represented as lists, with each of their 
 * digits as an element.
 * Implementation: Uses mulByDigit to multiply each element of l2 to the entirety of l1.  Then, uses
 * bigAdd to add all the products of [elements of l2 * l1]
 * Ex: bigMul [2;2;2;2] [3;3] -> will add 3 * 2222 and 3 *2222, with correct 0's added for levels.
 *)

let bigMul l1 l2 = 
  let f a x =  
    let (lvl, acc) = a in 
    let product = mulByDigit x l1 in 
    let product2 = List.append product (clone 0 lvl) in 
    let fproduct = bigAdd acc product2 in 
    (lvl+1, fproduct) in 
  let base = (0,[0]) in 
  let args = List.rev l2 in
  let (_, res) = List.fold_left f base args in
    res



let _= print_string "\nRen\n";;
let _= List.iter print_int (bigMul [3] [9;1]);;
let _= print_string "\nRen\n";;
let _= List.iter print_int (bigMul [0] [9;1]);;
