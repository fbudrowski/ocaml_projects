(*	Autor: Franciszek Budrowski
	Reviewer: Szymon Stolarczyk
*)

type wartosc = {l:float; r:float}

(* Ustawia do postaci normalnej - 
 * albo (l < p), albo (l > p),
 * albo (-inf,r), albo (l,inf), albo (nan,nan) *)
let normalizer x = 
	let lClass=classify_float x.l 
	and rClass=classify_float x.r in
	if lClass = FP_nan || rClass = FP_nan 
		then {l=nan; r=nan}
	else if lClass = FP_infinite && rClass = FP_infinite 
		then
		if x.l=infinity || x.r=neg_infinity 
			then {l=nan;r=nan;}
		else x
	else if lClass = FP_infinite then {l = neg_infinity; r = x.r}
	else if rClass = FP_infinite then {l=x.l;r=infinity}
	else x


let pustaWartosc = {l=nan; r=nan}

let pelnaWartosc = {l=neg_infinity; r=infinity}

let pusty x =
	let y = normalizer x in
		let lClass=classify_float y.l 
		and rClass=classify_float y.r 
		in
			lClass = FP_nan || rClass = FP_nan


let pelny x = 
	x.l = neg_infinity && x.r = infinity 


let eps = 1e-14
let zero x = 
	(-.eps) <= x.l && x.l <= eps && (-.eps) <= x.r && x.r <= eps 


let in_wartosc x y = 
	if x.l > x.r 
		then 
		if y <= x.r+.eps || x.l-.eps <= y 
			then true
		else false
	else 
		if x.l-.eps <= y && y <= x.r+.eps 
			then true
		else false

let przeciwna x = 
	if pusty x then x
	else 
		{l=(-.x.r);r=(-.x.l)}



let wartosc_dokladnosc x p =
	let odleglosc = abs_float (x *. p /. 100.) in
	{l = x -. odleglosc; r = x +. odleglosc}

let wartosc_od_do x y = {l=x; r=y}

let wartosc_dokladna x = {l=x; r=x}


let min_wartosc x=
	if pusty x then nan
	else if x.l > x.r then neg_infinity
	else 
		if x.l = (-.0.) then 0.
		else x.l

let max_wartosc x =
	if pusty x then nan
	else if x.l > x.r then infinity
	else 
		if x.r = (-.0.) then 0.
		else x.r
 
	

let sr_wartosc x = 
	let mini = min_wartosc x 
	and maxi = max_wartosc x
	in 
		let miniClass = classify_float mini
		and maxiClass = classify_float maxi
		in
			if miniClass = FP_infinite 
			&& maxiClass = FP_infinite
				then nan
			else 
				let ans = (mini +. maxi) /. 2. 
				in
					if ans = (-.0.) then 0. 
					else ans

let plus a0 b0 = 
	let a = normalizer a0 
	and b = normalizer b0 in
	if pusty a || pusty b
		then pustaWartosc
	else if pelny a || pelny b || (a.r < a.l && b.r < b.l)
		then pelnaWartosc
	else 
		let left = a.l +. b.l
		and right = a.r +. b.r
		in 
		if (a.r < a.l || b.r < b.l) && left <= right 
			then pelnaWartosc
		else 
			{l=left;r=right}

let minus a b = plus a (przeciwna b)

let rec get_best numberList acc comparer = 
	match numberList with
	| head::tail -> get_best tail (comparer head acc) comparer
	| [] -> acc

let max = (fun x y -> if x>y then x else y)
let min = (fun x y -> if x<y then x else y)

let doprazydop a b =
	if a.l < 0. || a.r > 0. || b.l < 0. || b.r > 0.
		then pelnaWartosc
	else 
		let rAns = max (a.l *. b.r) (a.r *. b.l)
		and lAns = min (a.l *. b.l) (a.r *. b.r)
		in
		if lAns <= rAns 
			then pelnaWartosc
		else
			{l=lAns;r=rAns}

let rec razy a0 b0 = 
	let a = normalizer a0
	and b = normalizer b0 in
	if pusty a || pusty b
		then pustaWartosc
	else if zero a || zero b (*zero*)
		then {l=0.;r=0.;}
	else if pelny a || pelny b
		then pelnaWartosc
	else 
	let mnozniki = [ a.l *. b.l; a.l *. b.r; a.r *. b.l; a.r *. b.r ]
	in
	if a.l > a.r && b.l > b.r (*dwa dopelnienia*)
		then doprazydop a b
	else
		(*let print = Printf.printf "NOT THE SAME %.3f %.3f;; %.3f %.3f\n" a.l a.r b.l b.r 
		in*)
		if a.l <= a.r && b.l <= b.r (*dwa ograniczone przedzialy*)
			then 
			let lAns = get_best mnozniki (a.l *. b.l) min
			and rAns = get_best mnozniki (a.l *. b.l) max
			in 
				{l=lAns;r=rAns}
		else (*jeden dopelnienie, drugi ograniczony*)
			mnozdopogr a b

and mnozdopogr a b =
	if a.l > a.r (* && b.l <= b.r *) 
		then 
		if b.l < 0. && 0. < b.r (* if b has zero*)
			then pelnaWartosc
		else if b.r > 0. (*if b is >0 *)
			then
			let lAns = min (a.l *. b.l) (a.l *. b.r)
			and rAns = max (a.r *. b.l) (a.r *. b.r)
			in
			if lAns <= rAns 
				then pelnaWartosc
			else
				{l=lAns;r=rAns}
		else (*if b is <0 *)
			razy {l = (-.a.r); r = (-.a.l)} {l = (-.b.r); r = (-.b.l)}
	else 
		razy b a

let podzielic a b0 = 
	let b = normalizer b0 in
	if pusty b 
		then pustaWartosc
	else if zero b 
		then pustaWartosc
	else if pelny a || pelny b
		then pelnaWartosc
	else 
		razy a { l = 1. /. b.r; r = 1. /. b.l }


(*Example: a: {l=1, r=0}, a*a -> full*)
