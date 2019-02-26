
open Arytmetyka;;

#trace razy;;
#trace podzielic;;

let is_nan x = compare x nan = 0;;

let a = wartosc_od_do (-1.) 1.            (* <-1, 1> *)
let b = wartosc_dokladna (-1.)            (* <-1, -1> *)
let c = podzielic b a                     (* (-inf -1> U <1 inf) *)
;;
let l = wartosc_od_do (-.2.) 0.;;
let k = podzielic l l;;
assert (in_wartosc a 1.);;
assert (in_wartosc a (-.1.));;
assert (in_wartosc a 0.);;
assert ((in_wartosc a 11.)=false);;
assert ((in_wartosc a infinity)=false)
assert (in_wartosc b (-.1.));;
assert ((in_wartosc b 0.)=false);;
assert ((in_wartosc c neg_infinity)=true);;


	

