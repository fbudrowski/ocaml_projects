(* 	Autor: Franciszek Budrowski
	Reviewer: Karolina Gabara
	License: MIT *)


type point = float * float

type kartka = point -> int

let prostokat (mnx, mny) (mxx, mxy) =
  function (x, y) -> 
      if mnx <= x && x <= mxx 
      && mny <= y && y <= mxy 
        then 1 else 0

let kolko (p1x, p1y) r = 
  function (x, y) ->
    let ox = (x -. p1x)
    and oy = (y -. p1y) in
      if ox *. ox +. oy *. oy <= r *. r 
        then 1 else 0

let vector_mult (p1x, p1y) (p2x, p2y) (p3x, p3y) =
  (p2x -. p1x) *. (p3y -. p2y) -. (p3x -. p2x) *. (p2y -. p1y)

let sym (x1, y1) (x2, y2) (x3, y3) =
  let (bx, by) = (x2 -. x1, y2 -. y1)
  and (px, py) = (x3 -. x1, y3 -. y1)
  in
    let (ansx, ansy) = ( (*b / p in complex (b = bx + i*by, p = px + i * py)*)
        (px *. bx +. py *. by) /. (bx *. bx +. by *. by),
      -.(py *. bx -. px *. by) /. (bx *. bx +. by *. by))
    in
      (x1 +. ansx *. bx -. ansy *. by,
       y1 +. ansy *. bx +. ansx *. by)

let eps = 10. ** (-.8.)

let zloz p1 p2 k =
  function p3 -> 
    if p1 = p2 then
      failwith "Symetria wzgledem punktu"
    else if vector_mult p1 p2 p3 < -.eps then 0
    else if vector_mult p1 p2 p3 < eps then k p3
    else
      (k p3) + (k (sym p1 p2 p3))
      
let skladaj ll k = 
  List.fold_left
  (fun k2 -> fun (p1, p2) -> zloz p1 p2 k2) k ll
