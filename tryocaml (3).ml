let even = [2;4;6;8;10] 
let odd = [1;3;5;7;9]
   (* This is an OCaml editor.
   Enter your program here and send it to the toplevel using the "Eval code"
   button or [Ctrl-e]. *) 



(****** 1. Empty Set ******) 

let emptyset () = [] ;;

(* The Empty set contains no element, hence it proves that it has no duplicate 
elemets, hence proving the given list is a set*)




(****** 2. Member x s ******)

let rec member x s = match s with 
    [] -> false
  | y::ys -> if x = y then true else member x ys ;;


member 3 even;;  (* returns false  as 3 is not in list even*)
member 2 even;;  (* returns true as 2 is in list even*)
(*Since 3 *)




(****** 3. Subser s1 s2 ******)
let rec subset s1 s2 = match s1 with 
    [] -> true 
  | x::xs -> if member x s2 then subset xs s2 else false ;;

subset [2;4] even ;; (* returns true as [2,4] is a set which is a subset of even*) 
subset [2;3] even ;; (* returns false as 3 is not a member of even and hence not
                     a subset of even*)

                     
                     
                     

(* 4. Equal s1 s2 *)

let rec equal s1 s2 = if subset s1 s2 && subset s2 s1 then true else false ;;


equal [] [] ;; (* returns true as both are empty set*)

(* 5. Union s1 s2*)

let rec union s1 s2 = match s1 with 
    [] -> s2 
  | x::xs -> 
      if member x s2 then 
        union xs s2 
      else
        x :: (union xs s2) ;;

union even odd ;;
      

(*
   
   Laws to prove : 
........................
a. Commutative (union s1 s2) 
-> base case : when s1 is empty then (union s1 s2) is s2 since every element in either s1 or s2 is in s2 
-> induction hypothesis : 
(union s1 s2 )  = (union s2 s1) 
if x is a member of s1 and not a member of s2 then
(union s1 s2) is x::(union xs s2)

   
   *)

(*6.Intersection s1 s2 *)

let rec intersection s1 s2 = match s1 with 
    [] -> []
  | x::xs -> 
      if member x s2 then 
        x:: intersection xs s2 
      else 
        intersection xs s2 ;;

intersection even [2;4;5] ;;




(* 

    intersection s1 s2 - returns the intersection of s1 and s2
    
    Proof of equational laws of sets

        Claim:      When s1 & s2 are sets it follows equational laws of sets ie.
                    a. Idempotent 
                    b. Identity 
                    c. Associative 
                    d. Commutative 
                    e. Distributive 

        Proof:      by induction on s1
        Case:       s1 = []             (emptyset)
                    for all s2
                    intersection s1 s2
                =   intersection [] s2
                =   []                  (evaluating the intersection function)
                    which proves the identity law of sets.

                =    Also if s1 = s2 then 
                =    intersection s1 s1 
                =    s1                  (evaluating the intersection function)
                    which proves the Idempotent law of sets.

        Case:       s1 = h :: t
        Induction Hypothesis:
         (I. H.)    for all s2, (intersection t s2) = intersection s2 t 
        Induction
        Step:
                    intersection s1 s2
                =   intersection (h :: t) s2
                    which acc. to the function can be broken into two cases:
                =   intersection t s2           (if h ∉ s2)
              
                    or:
                =   h :: (intersection t s2)    (if h ∈ s2)
    
                =   h :: (intersection s2 t)     (by IH)
                =   both are equal and hence follows commutative law of sets.

                    
*)


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

















