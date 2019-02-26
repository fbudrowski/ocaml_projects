(* Author: Franciszek Budrowski
   License: MIT *)
let rec gcd a b = 
  if a < b then
    gcd b a
  else if b = 0 then
    a
  else 
    gcd b (a mod b)

let pow a b = int_of_float ((float_of_int a) ** (float_of_int b))

let hashfun modulo multipl arr = 
  Array.fold_left (fun hash -> fun v -> (hash * multipl + v) mod modulo)
            0 arr

let op1 sizes cur pos = 
  (!cur).(pos) <- (!sizes).(pos);
  ()

let op2 cur pos = 
  (!cur).(pos) <- 0;
  ()

let op3 sizes cur p1 p2 =
  if p1 = p2 then ()
  else
    let change = min (!cur).(p1) ((!sizes).(p2) - (!cur).(p2))
    in
      (!cur).(p1) <- ((!cur).(p1) - change);
      (!cur).(p2) <- ((!cur).(p2) + change);
      ()

let tryAdding hash nexq visited supportState = 
  (*Printf.printf "MM %d\n" (hash !supportState);*)
  if Hashtbl.mem visited (!supportState, (hash !supportState)) then
    ()
  else(
    (*print_endline "TAK";*)
    Hashtbl.add visited (!supportState, (hash !supportState)) true;
    Queue.add !supportState !nexq;
  )

let equals a b = 
  let sa = Array.length a
  and sb = Array.length b
  and ans = ref true
  in
    if sa <> sb then false
    else (
      for i = 0 to sa - 1 do
        if a.(i) <> b.(i) then 
          ans := false;
      done;
      !ans
    )



let przelewanka t0 = 
  let numCases = Array.length t0
  and sizes = Array.map (fun (size, _) -> size) t0
  and targets = Array.map (fun (_, target) -> target) t0
  in
    let visited = Hashtbl.create (pow 2 (min 20 numCases))
    and hash = hashfun 1000696969 6037
    and curval = ref 0
    and curq = ref (Queue.create () )
    and nexq = ref (Queue.create () )
    and zeros = Array.make numCases 0
    and answer = ref (-1)
    and gcdsizes = Array.fold_left gcd 0 sizes
    and fulemps = Array.fold_left 
     (fun sum -> fun (size, target) -> 
       sum + (if size = target || target = 0 then 1 else 0))
     0 t0
    in
      for i = 0 to numCases - 1 do
        if gcdsizes <> 0 && (targets.(i) mod gcdsizes) <> 0 then
          answer := -2;
      done;
      if numCases > 0 && (!answer = -2 || fulemps = 0) then
        -1
      else (
      Queue.add zeros !curq;
      Hashtbl.add visited (zeros, (hash zeros)) true;
      while (!answer = -1) && 
      (((Queue.is_empty !curq) = false)
      || ((Queue.is_empty !nexq) = false)) do
        if (Queue.is_empty !curq) = true then (
          curq := !nexq;
          nexq := Queue.create ();
          incr curval 
        );
        (*Printf.printf "CV %d CQ %d NQ %d\n" !curval (Queue.length !curq) (Queue.length !nexq);*)
        let currentState = Queue.peek !curq
        and supportState = ref (Array.copy (Queue.pop !curq))
        in
          (*Printf.printf "State dist %d hash %d:: \n" !curval (hash currentState);
          for i = 0 to numCases - 1 do
            Printf.printf "\t%d c%d t%d s%d\n" i currentState.(i) targets.(i) sizes.(i);
          done;*)
          if equals currentState targets then 
            answer := !curval
          else (
            for i = 0 to numCases - 1 do
              for j = 0 to numCases - 1 do
                supportState := Array.copy currentState;
                op3 (ref sizes) supportState i j;
                tryAdding hash nexq visited supportState
              done;

              (*print_endline "One and two";*)
              supportState := Array.copy currentState;
              op1 (ref sizes) supportState i;
              tryAdding hash nexq visited supportState;
            
              supportState := Array.copy currentState;
              op2 supportState i;
              tryAdding hash nexq visited supportState;
            done;
          )
      done;
      !answer );;
      
      
