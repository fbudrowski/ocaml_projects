(* Franciszek Budrowski
   Review: Damian Chanko
   License: MIT *)


open PMap

exception Cykliczne

let getval x map = if (exists x map) then (find x map) else 0

(* ensures that v appears in the map *)
let ensure v appearances neilistmap = 
  if (exists v !appearances) then ()
  else (
    appearances := add v 0 !appearances;
    neilistmap := add v [] !neilistmap
  )

(* gets a FULL vertex list, including hidden vertices *)
let getNewVerlist appearances = 
  foldi 
  (fun key -> fun vlu -> fun acc -> (key :: acc))
  appearances []
  

(* fills appearance map and neighbor list pointer map *)
let rec fillAppearances appearances neilistmap verlist =   
  match verlist with
  | (v, neighs) :: tail -> (
    ensure v appearances neilistmap;
    neilistmap := add v (neighs @ (find v !neilistmap)) !neilistmap;
    let rec addneis neighs appearances = (
      match neighs with
      | h :: t -> (
        ensure h appearances neilistmap;
        appearances := add h ((getval h !appearances) + 1) !appearances;
        addneis t appearances
        )
      | [] -> ()
    )
    in
      addneis neighs appearances;
      fillAppearances appearances neilistmap tail
    )
  | [] -> ()

(* puts v in front of the topo order, removes v from everywhere *)
  let rec clear v appearances neilistmap answer =
    if (exists v !appearances) = false || (find v !appearances) <> 0 then
      ()
    else(
      answer := v :: !answer;
      appearances := remove v !appearances;
      let neilist = ref (find v !neilistmap)
      in
        while List.length !neilist <> 0 do
          match !neilist with
          | hd :: tl -> (
            let hdval = find hd !appearances in
              appearances := add hd (hdval - 1) !appearances;
              neilist := tl;
              if hdval = 1 then
                clear hd appearances neilistmap answer
            )
          | [] -> ()
        done
    )

       let rec clearAll appearances neilistmap answer verlist = 
         match verlist with
         | v :: tail -> (
           clear v appearances neilistmap answer;
           clearAll appearances neilistmap answer tail
           )
         | [] -> ()
       

(* does the toposort *)
let topol verlist = 
  let appearances = ref empty
  and neilistmap = ref empty
  and answer = ref [] 
  in
    fillAppearances appearances neilistmap verlist;
    let newVerlist = getNewVerlist !appearances
    in
      let vertexCount = List.length newVerlist
      in
        clearAll appearances neilistmap answer newVerlist;
        if List.length !answer <> vertexCount then
          raise Cykliczne
        else 
          List.rev !answer
