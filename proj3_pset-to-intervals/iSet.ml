(*
 * Coder: Franciszek Budrowski
 * Reviewer: Rafal Lyzwa
 *
 * ISet - Interval sets
 * Copyright (C) 1996-2017 Xavier Leroy, Nicolas Cannasse, Markus Mottl, Franciszek Budrowski
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

type 'k set =
  | Empty
  | Node of 'k set * 'k * 'k set * (int * int) (*Last parameter changed to (height, count) *)


type t =
  {
    cmp : int * int -> int * int -> int;
    set : (int * int) set;
  }

let height = function
  | Node (_, _, _, (h, _) ) -> h
  | Empty -> 0

let ctheight l r = max (height l) (height r) + 1

let count = function
  | Node (_, _, _, (_, c) ) -> c
  | Empty -> 0

let ctcount l k r = (snd k) - (fst k) + 1 + count l + count r

let countpar l k r = (ctheight l r, ctcount l k r)

let make l k r = Node (l, k, r, countpar l k r)

let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else Node (l, k, r, countpar l k r)

let rec min_elt = function
  | Node (Empty, k, _, _) -> k
  | Node (l, _, _, _) -> min_elt l
  | Empty -> raise Not_found

let rec remove_min_elt = function
  | Node (Empty, _, r, _) -> r
  | Node (l, k, r, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "PSet.remove_min_elt"

let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k = min_elt t2 in
      bal t1 k (remove_min_elt t2)

(*Create cmp removed - not needed. New comparing function*)
let comparer (a,b) (c,d) = 
  if b <> max_int && b + 1 < c then -1
  else if b < c then -2
  else if d <> max_int && d + 1 < a then 1
  else if d < a then 2
  else 0

let mergevals (a,b) (c,d) =
  (min a c), (max b d)




let empty = { 
  cmp = comparer;
  set = Empty }

let is_empty x = 
  x.set = Empty
  

let rec clear_l mgb x = function
  | Node(l, k, r, h) -> (
      if mgb x k then
        let ltree, lval = clear_l mgb x l in
          (ltree, mergevals lval (mergevals x k))
      else
        let rtree, rval = clear_l mgb x r in
          (bal l k rtree, mergevals rval x)
      )
  | Empty -> (Empty, x)

let rec clear_r mgb x = function
  | Node(l, k, r, h) ->
      if mgb x k then
        let rtree, rval = clear_r mgb x r in
          (rtree, mergevals rval (mergevals x k))
      else
        let ltree, lval = clear_r mgb x l in
          (bal ltree k r, mergevals lval x)
  | Empty -> (Empty, x)


let rec add_one cmp x = function
  | Node (l, k, r, h) ->
      let c = cmp x k 
      and mergable cmp (a,b) (c,d) = 
        let ans = cmp (a,b) (c,d) 
        in
          ans <> 1 && ans <> -1
      in
      if mergable cmp x k then 
        let xtm = mergevals x k
        in
          let ltree, lx = clear_l (mergable cmp) xtm l
          and rtree, rx = clear_r (mergable cmp) xtm r
          in
            let xf = mergevals xtm (mergevals lx rx)
            in
              bal ltree xf rtree
      else if c < 0 then
        let nl = add_one cmp x l in
        bal nl k r
      else
        let nr = add_one cmp x r in
        bal l k nr
  | Empty -> Node (Empty, x, Empty, (1, ctcount Empty x Empty) )

let add x { cmp = cmp; set = set } =
  { cmp = cmp; set = add_one cmp x set }

let rec join cmp l v r = (*Slightly changed parameter*)
  match (l, r) with
    (Empty, _) -> add_one cmp v r
  | (_, Empty) -> add_one cmp v l
  | (Node(ll, lv, lr, (lh, lc)), Node(rl, rv, rr, (rh, rc))) ->
      if lh > rh + 2 then bal ll lv (join cmp lr v r) else
      if rh > lh + 2 then bal (join cmp l v rl) rv rr else
      make l v r

let split x { cmp = cmp; set = set } =
  let rec loop x = function
      Empty ->
        (Empty, false, Empty)
    | Node (l, v, r, _) ->
        let c = cmp (x, x) v in
        if c = 0 then (
          (
            if (fst v) == x then 
              l
            else
              (add_one cmp (fst v, x - 1) l)
          ),
          true,
          (
            if (snd v) == x then
              r
            else
              (add_one cmp (x + 1, snd v) r)
          )
        )
        else if c < 0 then
          let (ll, pres, rl) = loop x l in (ll, pres, join cmp rl v r)
        else
          let (lr, pres, rr) = loop x r in (join cmp l v lr, pres, rr)
  in
  let setl, pres, setr = loop x set in
  { cmp = cmp; set = setl }, pres, { cmp = cmp; set = setr }

let remove x { cmp = cmp; set = set } =
  let rec loop = function
    | Node (l, k, r, _) ->
        let c = cmp x k in
        if c = 0 then (
          let mgb cmp a b = ((cmp a b) = 0) in
            let ltree, lval = clear_l (mgb cmp) x l
            and rtree, rval = clear_r (mgb cmp) x r in
              let xfin = mergevals (mergevals x k) (mergevals lval rval) in
              if (fst xfin) < (fst x) then 
                if (snd x) < (snd xfin) then
                  add_one cmp ((snd x) + 1, snd xfin) (bal ltree (fst xfin, (fst x) - 1) rtree)
                else bal ltree (fst xfin, (fst x) - 1) rtree
              else if (snd x) < (snd xfin) then
                bal ltree ((snd x) + 1, snd xfin) rtree
              else merge ltree rtree
        )
        else if c < 0 then 
          bal (loop l) k r
        else
          bal l k (loop r)
    | Empty -> Empty 
  in
    { cmp = cmp; set = loop set }

let exists x { cmp = cmp; set = set } =
  let rec loop = function
    | Node (l, k, r, _) ->
        let c = cmp x k in
        c = 0 || loop (if c < 0 then l else r)
    | Empty -> false in
  loop set

let mem x = exists (x, x)

let iter f { set = set } =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _) -> loop l; f k; loop r in
  loop set

let fold f { cmp = cmp; set = set } acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _) ->
          loop (f k (loop acc l)) r in
  loop acc set

let elements { set = set } = 
  let rec loop acc = function
      Empty -> acc
    | Node(l, k, r, _) -> loop (k :: loop acc r) l in
  loop [] set

(*New function below. Supposes that integers flip when they overflow*)
let below x {set = set; cmp = cmp} =
  let rec loop = function
    | Empty -> 0
    | Node(l, k, r, (_, ct) ) ->
      let c = cmp (x, x) k in
        if c < 0 then loop l
        else if c > 0 then ct - (count r) + loop r
        else 
          (count l) + x - (fst k) + 1
  in
    let ans = loop set in
      match set with
      | Empty -> ans
      | Node(_, k, _, _) -> 
        if ans < 0 || (ans = 0 && k = (min_int, max_int)) then
          max_int 
        else
          ans
 
