open Arytmetyka;;

let is_nan x = compare x nan = 0;;

let a = wartosc_od_do (-1.) 1.            (* <-1, 1> *)
let b = wartosc_dokladna (-1.)            (* <-1, -1> *)
let c = podzielic b a                     (* (-inf -1> U <1 inf) *)
let d = plus c a                          (* (-inf, inf) *)
let e = wartosc_dokladna 0.               (* <0, 0> *)
let f = razy c e                          (* <0, 0> *)
let g = razy d e                          (* <0, 0> *)
let h = wartosc_dokladnosc (-10.) 50.     (* <-15, -5> *)
let i = podzielic h e                     (* nan, przedzial pusty*)
let j = wartosc_od_do (-6.) 5.            (* <-6, 5> *)
let k = razy j j                          (* <-30, 36> *)
let l = plus a b                          (* <-2, 0> *)
let m = razy b l                          (* <0, -2> *)
let n = podzielic l l                     (* <0, inf) *)
let o = podzielic l m                     (* (-inf, 0) *)
let p = razy o a                          (* (-inf, inf) *)
let q = plus n o                          (* (-inf, inf) *)
let r = minus n n                         (* (-inf, inf) *)
let s = wartosc_dokladnosc (-0.0001) 100. (* <-0.0002, 0> *)
let t = razy n s                          (* (-inf, 0) *)
;;

assert ((min_wartosc c, max_wartosc c) = (neg_infinity, infinity));
assert (compare (sr_wartosc c) nan = 0); (*compare a b = {1 gdy a>b, 0 gdy a=b, -1 gdy a<b}, w przeciwienstwie do "==" "compare" ogarnia porownywanie nan*)
assert (in_wartosc c 0. = false);
assert ((in_wartosc c (-1.)) && (in_wartosc c (-100000.)) && (in_wartosc c 1.) && (in_wartosc c 100000.));
assert ((in_wartosc d 0.) && (in_wartosc d (-1.)) && (in_wartosc d (-100000.)) && (in_wartosc d 1.) && (in_wartosc d 100000.));
assert ((min_wartosc f, max_wartosc f, sr_wartosc f) = (0., 0., 0.));
assert ((min_wartosc g, max_wartosc g, sr_wartosc g) = (0., 0., 0.));
assert ((min_wartosc h, max_wartosc h, sr_wartosc h) = (-15., -5., -10.));
assert ((compare nan (min_wartosc i), compare nan (sr_wartosc i), compare nan (max_wartosc i)) = (0, 0, 0));
assert ((min_wartosc k, max_wartosc k, sr_wartosc k) = (-30., 36., 3.));
assert ((min_wartosc n, max_wartosc n, sr_wartosc n) = (0., infinity, infinity));
assert ((min_wartosc o, max_wartosc o, sr_wartosc o) = (neg_infinity, 0., neg_infinity));
assert ((min_wartosc p, max_wartosc p, compare (sr_wartosc p) nan) = (neg_infinity, infinity, 0));
assert ((min_wartosc q, max_wartosc q, compare (sr_wartosc q) nan) = (neg_infinity, infinity, 0));
assert ((min_wartosc r, max_wartosc r, compare (sr_wartosc r) nan) = (neg_infinity, infinity, 0));
assert ((min_wartosc t, max_wartosc t, sr_wartosc t) = (neg_infinity, 0., neg_infinity));;

let a = wartosc_od_do neg_infinity infinity
let b = a
let c = plus a b
let d = razy a b
let e = podzielic a b
let f = minus a b;;

assert((min_wartosc c, max_wartosc c, is_nan (sr_wartosc c)) = (neg_infinity,infinity,true));
assert((min_wartosc d, max_wartosc d, is_nan (sr_wartosc d)) = (neg_infinity,infinity,true));
assert((min_wartosc e, max_wartosc e, is_nan (sr_wartosc e)) = (neg_infinity,infinity,true));
assert((min_wartosc d, max_wartosc d, is_nan (sr_wartosc d)) = (neg_infinity,infinity,true));;

let a = wartosc_od_do 0. infinity
let b = wartosc_dokladna 0.
let c = podzielic a b
let d = podzielic  b b;;
assert((is_nan(min_wartosc c), is_nan(max_wartosc c), is_nan (sr_wartosc c)) = (true,true,true));
assert((is_nan(min_wartosc d), is_nan(max_wartosc d), is_nan (sr_wartosc d)) = (true,true,true));;

let a = wartosc_od_do (-10.) 10.
let b = wartosc_od_do (-1.) 1000.
let c = podzielic a b;;
assert((min_wartosc c, max_wartosc c, is_nan (sr_wartosc c)) = (neg_infinity,infinity,true));;

let a = wartosc_od_do (-1.0) 1.0
let b = wartosc_dokladna 1.0
let c = podzielic b a
let d = wartosc_dokladna 3.0
let e = plus c d (* e = -inf,2.0 4.0 inf *)
let f = podzielic b e (* f = -inf,1/4 1/2, inf*)
let g = podzielic d a (* g = -inf,-3 3,inf *)
let h = podzielic g f (*h = -inf,inf*)
let i = plus f g;; (*i = -inf,inf*)

assert((in_wartosc f 0.25,in_wartosc f 0.26,in_wartosc f 0.49,in_wartosc f 0.50)=(true,false,false,true));
assert((min_wartosc h, max_wartosc h, is_nan (sr_wartosc h),in_wartosc h 0.) = (neg_infinity,infinity,true,true));
assert((min_wartosc h, max_wartosc h, is_nan (sr_wartosc h),in_wartosc h 0.3) = (neg_infinity,infinity,true,true));;

assert(in_wartosc (wartosc_dokladna 2.0) 2.0);
assert(in_wartosc (wartosc_od_do 1.0 3.0) 2.0);
assert(in_wartosc (wartosc_od_do 2.0 3.0) 2.0);
assert(1. = sr_wartosc (wartosc_od_do 0. 2.));
assert(not (in_wartosc (wartosc_od_do 2.0 3.0) 4.0));
assert(wartosc_od_do 1. 5. = plus (wartosc_od_do 0.0 2.0) (wartosc_od_do 1.0 3.0));
assert(3. = sr_wartosc (plus (wartosc_od_do 0.0 2.0) (wartosc_od_do 1.0 3.0)));
assert(wartosc_od_do (-3.) 1. = plus (wartosc_od_do 0.0 2.0) (wartosc_od_do (-3.0) (-1.0)));
assert(wartosc_od_do (-3.) 1. = minus (wartosc_od_do 0.0 2.0) (wartosc_od_do 1. 3.));
assert(wartosc_od_do 0. 6. = razy (wartosc_od_do 0.0 2.0) (wartosc_od_do 1.0 3.0));
assert(wartosc_od_do (-6.) 6. = razy (wartosc_od_do (-2.0) 2.0) (wartosc_od_do 1.0 3.0));
assert(0. = sr_wartosc (razy (wartosc_od_do (-2.0) 2.0) (wartosc_od_do 1.0 3.0)));
assert(wartosc_od_do 0. 2. = podzielic (wartosc_od_do 0.0 2.0) (wartosc_od_do 1.0 3.0));
assert(wartosc_od_do (1. /. 3.) 1. = podzielic (wartosc_od_do 1.0 2.0) (wartosc_od_do 2.0 3.0));
assert(wartosc_od_do 1. infinity = podzielic (wartosc_od_do 1.0 2.0) (wartosc_od_do 0.0 1.0));
assert(wartosc_od_do neg_infinity (-1.) = podzielic (wartosc_od_do 1.0 2.0) (wartosc_od_do (-1.0) 0.0));;

let jed = wartosc_dokladna 1.
let zero = wartosc_dokladna 0.

let a = zero;;
assert((sr_wartosc a, max_wartosc a, min_wartosc a) = (0.,0.,0.));;

let a = wartosc_od_do 0. 1. (*(0,1)*)
let b = podzielic a a (*0, inf*)
let c = razy b zero;; (*0,0*)
assert((sr_wartosc c, max_wartosc c, min_wartosc c) = (0.,0.,0.));;

let a = podzielic jed zero;; (*nan*)
assert(compare (min_wartosc a) nan = 0);
assert(compare (max_wartosc a) nan = 0);
assert(compare (sr_wartosc a) nan = 0);;

let a = wartosc_dokladnosc 1. 110.;; (*-0.1, 2.1*)
assert(in_wartosc a (-.0.1));
assert(in_wartosc a (2.1));;

let a = wartosc_od_do (-.3.) 0.
let b = wartosc_od_do 0. 1.
let c = podzielic a b;; (*-inf, 0*)
assert(max_wartosc c = 0.);
assert(min_wartosc c = neg_infinity);
assert(sr_wartosc c = neg_infinity);;

let a = wartosc_od_do 1. 4.
let b = wartosc_od_do (-.2.) 3.
let c = podzielic a b (*(-inf, -1/2) U (1/3, inf) *)
let d = podzielic c b (*(-inf, -1/6) U (1/9, inf) *)
let e = plus d jed (*(-inf, 5/6) U (10/9, inf)*)
let f = sr_wartosc (podzielic jed (wartosc_dokladna 9.));;(*1/9*)
assert(compare (sr_wartosc d) nan = 0);
assert(in_wartosc d 0.12);
assert(in_wartosc d 0. = false);
assert(in_wartosc d (-0.125) = false);
assert(in_wartosc d f = true);
assert(in_wartosc e 1. = false);;

