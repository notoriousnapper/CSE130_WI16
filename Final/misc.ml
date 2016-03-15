

let rec insert l i = 
    match l with 
    | [] -> [i]
    | h::t -> if (i<=h) then i::l
         	else h :: (insert t i)
(*if (i > h) then *)


let insertion_sort = 
	fun l i -> 
	List.fold_left insert [] l 
(* Explanation again *)
 
    

let test = [1;2;3;4];;
let x = 5;;

let y = insert test 3;;
let y = insert [10;15;20;30] 5;;


List.iter (print_int) y

(*let test = [1;2;3;4]
let test = [1;2;3;4]
insert test 3*)
