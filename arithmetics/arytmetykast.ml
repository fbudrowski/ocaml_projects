(*	Autor: Franciszek Budrowski
	Reviewer: Szymon Stolarczyk
*)

type przedzial = {l: float; r: float}
type wartosc = przedzial list

let rec popr_wartosc_p stary nowy = 
	(* zmienia liste przedzialow posortowanych po poczatkach na liste przedzialow rozlacznych*)
	match stary with
	| hStary::tStary -> (
		if hStary.l == nan || hStary.r == nan then popr_wartosc_p tStary nowy
		else
		match nowy with
		| hNowy::tNowy -> 
			popr_wartosc_p tStary (
				if hNowy.r < hStary.l then popr_wartosc_p tStary (hStary::nowy)
				else
					if hStary.r < hNowy.r then popr_wartosc_p tStary nowy 
					else popr_wartosc_p tStary ({l=hNowy.l;r=hStary.r}::tNowy)
			)
		| [] -> 
			popr_wartosc_p tStary (hStary::nowy)
		)
	| [] -> nowy


let popr_wartosc stary =
	List.rev (popr_wartosc_p stary [])



let rec przeciwna wartosc acc = (*wyznacza wartosc przeciwna*)
	match wartosc with
	| hPrz :: tPrz -> 
		przeciwna tPrz ({l=(0.-.hPrz.r);r=(0.-.hPrz.l)}::acc)
	| [] -> 
		acc


let eps=1e-11

let rec wartosc_dokladnosc x p = 	
	if x > 0. then
		[{l= x *. (100. -. p ) /. 100.; r = x *. (100. +. p) /. 100.}]
	else 
		przeciwna (wartosc_dokladnosc (-.x) p) []

let wartosc_od_do x y =
	[{l=x; r=y;}]
let wartosc_dokladna x = 
	[{l=x; r=x;}]


let rec in_wartosc w y = 
	match w with 
	| head::tail -> 
		if head.l == nan || head.r == nan then in_wartosc tail y
		else if head.l -. eps < y && y < head.r +. eps then true
		else in_wartosc tail y
	| [] -> false


let min_wartosc w= 
	match w with
	| head::tail -> head.l
	| [] -> nan

let max_wartosc w = 
	let reversed = List.rev w in
	match reversed with
	| head::tail -> head.r
	| [] -> nan


let sr_wartosc w =
	let mini = min_wartosc w 
	and maxi = max_wartosc w in
	
	if mini == nan || maxi == nan then nan
	else if mini=maxi then mini
	else if classify_float mini == FP_infinite 
	|| classify_float maxi == FP_infinite then nan
	else (mini +. maxi) /. 2.


let v = [{l=(-.10.);r=(-.8.)};{l=(-.1.);r=(10.)}]

let rec dodaj_przedzial ansloc acc0 acc = 
	match acc0 with
	| head::tail -> 
		if ansloc.l < head.l then 
			popr_wartosc ((List.rev acc0) @ (ansloc::acc))
		else 
			dodaj_przedzial ansloc tail (head::acc)
	| [] -> (
		popr_wartosc (List.rev (ansloc::acc))
		)






let rec get_best numberList acc comparer = 
	match numberList with
	| head::tail -> get_best tail (comparer head acc) comparer
	| [] -> acc

let max = (fun x y -> if x>y then x else y)
let min = (fun x y -> if x<y then x else y)

let nowe_przedzialy_mnoz a b =
	if a.l==nan || a.r == nan || b.l == nan || b.r == nan then []
	
	else
	let listaEkstremow = [ a.l*.b.l; a.l*.b.r; a.r*.b.l; a.r*.b.r ] in 
	let mini = get_best listaEkstremow (a.l*.b.l) min
	and maxi = get_best listaEkstremow (a.l*.b.l) max in
	
	[{l=mini;r=maxi}]
	

let rec nowe_przedzialy_dziel a b = 
	if a.l==nan || a.r == nan || b.l == nan || b.r == nan then []
	else if (a.l<=0. && 0.<=a.r && b.l<=0. && 0. <= b.r) then 
		[{l=neg_infinity;r=infinity}]
	else
	if b.l <= 0. && 0. <= b.r then
		if a.r > 0. then nowe_przedzialy_dziel {l=(-.a.r);r=(-.a.l)} b
		else
			[
			(if classify_float b.r = FP_zero || classify_float b.r = FP_subnormal
				then {l=neg_infinity;r=neg_infinity}
				else { l=neg_infinity; r= ( max (a.r/.b.r) (a.l/.b.r) )}
			);
			(if b.l > (-.eps) || classify_float b.l = FP_zero || classify_float b.l = FP_subnormal
				then { l=infinity;r=infinity}
				else 
					{ l= ( min (a.l/.b.l) (a.r/.b.l) ); r=infinity }
			)
			]
	else
		nowe_przedzialy_mnoz a {l=1./.b.r; r=1./.b.l}


let nowe_przedzialy oper a b = 

	if a.l==nan || a.r==nan || b.l==nan || b.r==nan then []
	else if oper = '+' then
		[{l=a.l+.b.l; r=a.r+.b.r}]
	else if oper = '*' then
		nowe_przedzialy_mnoz a b
	else
		nowe_przedzialy_dziel a b

			
		

let rec dodaj_pom2 acc listaPrzedzialow = 
	match listaPrzedzialow with
	| head :: tail -> dodaj_pom2 (dodaj_przedzial head acc []) tail
	| [] -> acc


let rec dodaj_pom1 oper a aLoc b0 bLoc acc = 
	match bLoc with
	| hBLoc :: tBLoc ->
		(
		let nowePrzedzialy= nowe_przedzialy oper aLoc hBLoc in
		dodaj_pom1 oper a aLoc b0 tBLoc (dodaj_pom2 acc nowePrzedzialy)
		)
	| [] ->
		(
		match a with
		| hA :: tA -> dodaj_pom1 oper tA hA b0 b0 acc
		| [] -> acc
		)


let plus a b = 		( dodaj_pom1 '+' a {l=nan;r=nan} b [] [] )

let minus a b = 	( dodaj_pom1 '+' a {l=nan;r=nan} (przeciwna b []) [] [] )

let razy a b = 		( dodaj_pom1 '*' a {l=nan;r=nan} b [] [] )
let podzielic a b = ( dodaj_pom1 '/' a {l=nan;r=nan} b [] [] )
;;

#trace dodaj_pom1;;
#trace dodaj_pom2;;
#trace dodaj_przedzial;;
#trace popr_wartosc;;
#trace popr_wartosc_p;;

nowe_przedzialy_dziel {l=(-.1.);r=(-.1.)} {l=(-.1.);r=(1.)};;
dodaj_pom2 [] [{l = neg_infinity; r = -1.}; {l = 1.; r = infinity}];;

popr_wartosc [{l = neg_infinity; r = -1.}; {l = 1.; r = infinity}];;
