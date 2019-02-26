(* 	Autor: Franciszek Budrowski
	Reviewer: Jakub Boguta *)



type 'a queue = 
	| None
	| Queue of 'a * int * 'a queue * 'a queue
	;;
(*
type 'a nonEmptyQueue = 
	{
		value : 'a;
		rightPathLen : 'a;
		lSon : 'a queue;
		rSon : 'a queue
	}
;;*)

let empty = None;;

let get_Right_Path a =
	match a with
	| None -> 0
	| Queue (value, rightPathLen, lSon, rSon) -> rightPathLen
;;

let is_empty a =
(
	match a with
	| None -> true
	| _ -> false
)
;;

let rec join q1 q2 = 
(
	match q1 with 
	| None -> q2
	| Queue (value1, rightPathLen1, lSon1, rSon1) -> (
		match q2 with
		| None -> q1
		| Queue (value2, rightPathLen2, lSon2, rSon2) -> (
			if value2 < value1 then
				join q2 q1
			else
				let leftSubtree = lSon1
				and rightSubtree = join rSon1 q2
				in
					let leftSize = get_Right_Path leftSubtree
					and rightSize = get_Right_Path rightSubtree
					in
						if leftSize < rightSize then
							Queue (value1, leftSize + 1,
							rightSubtree, leftSubtree)
						else
							Queue (value1, rightSize + 1,
							leftSubtree, rightSubtree)
		)
	)
);;

let add a q =
	let q2 = Queue (a, 1, None, None)
	in
		join q q2
;;

exception Empty;;

let delete_min q =
	match q with
	| None -> raise (Empty)
	| Queue (value, rightPathLen, lSon, rSon) -> 
		( value, join lSon rSon )
;;
