(* CSE 130: Programming Assignment 2
 * misc.ml
 * @ Jesse Ren A10650321
 *)


(*
    function: assoc
    Input:        (int*string*[int, string])  =>   ( a * b * [type a, type b])
    Big Idea: Function that searches with a given list for a specific 
    	string, and returns the
   	 int associated with it.  Else, it returns the default value given 
    	in its parameters.
    Algorithm:
    Base Case: If List is empty, Return default
    Avg Case:   If not empty, Recursively look through list using 
    	tail recursion, involving helper function 
*)
let rec assoc (d,k,l) = 
  match l with
  | [] -> d
  | (data,key)::t -> 
    if data = k then key 
    else assoc (d, k, t)

(*
    function: removeDuplicates
    Input:   'a list -> 'a list     
    Big Idea: Given a list l, returns a copy of l with no duplicates
    Algorithm: Sort element into new array, and if element to be inserted
	       has already been seen, don't add
*)

let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
      | [] -> seen
      | h::t -> 
        let seen' = if (List.mem h seen) then seen else (h :: seen) in
        let rest' = t in 
	  helper (seen',rest') 
  in
      List.rev (helper ([],l))


(*
    function: wwhile
    Input:   ('a -> 'a * bool) * 'a -> 'a
    Big Idea: Tail Recursive While Loop
    Algorithm: Since a function f returns a pair ('b, bool), f will be 
               continuously called until c' is bool is false. 
*)
let rec wwhile (f,b) = 
  let (b',c) = f b in
  	if c then wwhile (f,b')
  	else b'

(*
    function: fixpoint
    Input:    ('a -> 'a) * 'a -> 'a
    Big Idea: given a function f, recursively runs it on the result of an input
		and itself, until b = (fb)
    Algorithm: Repeatedly update b with (fb) until input is equal to f (input)
*)
let fixpoint (f,b) = 
	wwhile (
	  let temp b =
	    let c = f b in 
	      if c = b then 
			(c, false)
	      else 	
			(c, true)
	  	in temp
		,b)


let rec ffor (low,high,f) = 
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
      
(************** Add Testing Code Here ***************)
