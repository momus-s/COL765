let even = [2;4;6;8;10] 
let odd = [1;3;5;7;9]
   (* This is an OCaml editor.
   Enter your program here and send it to the toplevel using the "Eval code"
   button or [Ctrl-e]. *) 
(* 1. Empty Set *) 

let emptyset () = [] ;;

(* 2. Member x s*)

let rec member x s = match s with 
    [] -> false
  | y::ys -> if x = y then true else member x ys ;;


member 3 even;;
member 2 even;;

(* 3. Subser s1 s2*)
let rec subset s1 s2 = match s1 with 
    [] -> true 
  | x::xs -> if member x s2 then subset xs s2 else false ;;

(* 4. Equal s1 s2 *)

let rec equal s1 s2 = if subset s1 s2 && subset s2 s1 then true else false ;;

(* 5. Union s1 s2*)

let rec union s1 s2 = match s1 with 
    [] -> s2 
  | x::xs -> 
      if member x s2 then 
        union xs s2 
      else
        x :: (union xs s2) ;;

union even odd ;;
      

(*6.Intersection s1 s2 *)

let rec intersection s1 s2 = match s1 with 
    [] -> []
  | x::xs -> 
      if member x s2 then 
        x:: intersection xs s2 
      else 
        intersection xs s2 ;;

intersection even [2;4;5] ;;


(* 7.DIfference s1 s2*)

let rec diff s1 s2 = match s1 with 
    [] -> [] 
  | x::xs ->
      if member x s2 then 
        diff xs s2 
      else x::(diff xs s2) ;;

diff even [2;3;4;5];;


(* 8. Power s*)

let rec power s = match s with 
    [] -> [[]]
  | x::xs -> let ps = power xs in ps @ (List.map(fun y -> x::y)ps);;

power [3;4;2]



















