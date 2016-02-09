(*1b)*)



type expr = 
    |Const of int
    | Var of string
    | Op of string * expr * expr

let to_str e = 
let rec str_helper e top_level = 
    match e with 
    |Var(a)-> a 
    |Const(a) -> string_of_int a 
    |Op(a,b,c)-> if (top_level) then 
	let top_level = false in (str_helper b top_level)^a^(str_helper c top_level)
     else "("^(str_helper b top_level)^a^(str_helper c top_level)^")"
in str_helper e true

(*Problem 2*)
let average_if f l =
let folding_fn a x = 
    let (newn, acc) = a in 
    if (f x) then 
    (newn+1, acc + x)
    else 
    (newn, acc)
    in 
    let base = (0, 0) in
    let (n,res)= List.fold_left folding_fn base l in 
if (n=0)
then 0
else
(res/n)


(* Problem 3a *)

let length_2 l =
List.fold_left (+) 0 (List.map List.length l)

(*List.fold_left (funct) acc base list*)

(* Problem 3b *)

let length_3 l = 
List.fold_left (+) 0 (List.map length_2 l)




let f1 = List.map (fun x->2*x);;
f1 [1;2;3;4];; 
let f2 = List.fold_left (fun x y -> (y+2)::x) [];;
f2 [3;5;7;9];; 
let f3 = List.fold_left (fun x y -> x@[3*y]) [];;
f3 [1;3;6];;
(* This is going to get harder now... *)
let f = List.fold_left (fun x y -> y x);;
(*f 1 [(+) 1; (-) 2];; *)
(* Ok, this one is insanely hard!!! *)
let _= List.iter print_int (f [1;2;3] [f1;f2;f3]);;







type 'a fun_tree = 
| Leaf of ('a->'a)
| Node of ('a fun_tree) * ('a fun_tree)

(*
let rec apply_all t x = match t with 
| Node(a,b) -> (apply_all b ((apply_all a) x))
| Leaf(c) -> c 
*)



let rec apply_all t x =
  match t with
  | Leaf f -> f x
  | Node (l, r) -> apply_all r (apply_all l x);;

let f1 x = x + 1;;
let f2 x = x * 2;;
let f3 x = x + 3;;
let t = Node(Leaf f1, Node(Leaf f2, Leaf f3));;
apply_all t 0;;

let _= print_int (apply_all t 0);;



(* 3b *)
let f1 = (+) 1;;
let f2 = (-) 2;;
let f3 = (+) 3;;
let t = Node(Node(Leaf f1, Leaf f2), Leaf f3);;
apply_all t 0;;
let _= print_int (apply_all t 0);;

let f1 = (^) "a";;
let f2 x = x ^ "b";;
let f3 x = x ^ "ab";;
let t = Node(Leaf f1, Node(Leaf f2, Leaf f3));;
apply_all t "123";;
let _= print_string (apply_all t "123");;

let f1 = List.fold_left (fun x y -> (y*2)::x) [];;
let f2 = List.fold_left (fun x y -> x@[y]) [];;
let t = Node(Node(Leaf f1, Leaf f1), Node(Leaf f1, Leaf f2));;
apply_all t [1;2;3];;

let _= List.iter print_int (apply_all t [1;2;3]);;




let count l x = 
let f a h = if (x=h) then 
(a+1) else a in  
let b = 0 in 
List.fold_left f b l




let make_palyndrome l = 
    let f a h = h::a in 
    let b = l in 
    List.fold_left f b l 


