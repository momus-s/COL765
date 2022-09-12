(*In this programming assignment, you will define as data type a small language
  of propositions (given below) and write simple programs to manipulate them.*)

type prop = T | F  (* propositional constants *)

          | Atom of string   (* propositional variables *)

          | Not of prop   (* negation *)

          | And of prop * prop  (* conjunction *)

          | Or of prop * prop    (* disjunction *)

          | Imply of prop * prop   (* Implication *)

          | Iff of prop * prop    (* Bi-implication or Iff or equivalence *)

;;

(*
   Implement the following  

1. ht: prop -> int, which returns the height of a proposition (seen as a syntax tree).
2. size: prop -> int, which returns the number of nodes in a proposition (seen as a syntax tree).
3. atoms: prop -> string set, which returns the set of propositional variables that appear in a proposition
4. truth: prop -> (string -> bool) -> bool, which evaluates a proposition with respect to a given truth assignment to atoms
5. nnf: prop -> prop, which converts a proposition into its negation normal form.  Check for a few propositions that your program nnf preserves truth.
6. cnf: prop -> prop set set, which converts a proposition into conjunctive normal form (CNF or Product of Sums) represented as a set of clauses, each clause of which is a set of literals (a subset of prop).
7. truthcnf: prop set set -> (string -> bool) -> bool, which evaluates a given proposition in CNF, with respect to a truth assignment.  Check for a few propositions and truth assignments that your program cnf preserves truth.
8. dnf: prop -> prop set set,  which converts a proposition into disjunctive normal form (DNF or Sum of Products) as a set of terms, each of which is a set of literals (a subset of prop)
9. truthdnf: prop set set -> (string -> bool) -> bool, which evaluates a given proposition in DNF, with respect to a truth assignment. Check for a few propositions and truth assignments that your program dnf preserves truth.
   
   *)
  
  
(*Some Useful Functions*)

let rec member x s = List.mem x s ;;


(*Helper Function , Distributive over Or*)

let rec dist p1 p2 = 
  match (p1, p2) with
  | (a, And (b, c)) -> And (dist a b, dist a c)
  | (And (a, b), c) -> And (dist a c, dist b c)
  | (a,b)           -> Or (a,b)
;;



   
   
   
(* 
1. ht: prop -> int
*)

let rec ht p =
  match p with
  | Atom _           -> 0
  | T | F            -> 0
  | Not p1           -> 1 + ht(p1)
  | And (p1, p2)     -> 1 + max(ht p1) (ht p2)
  | Or (p1, p2)      -> 1 + max(ht p1) (ht p2) 
  | Imply (p1, p2)   -> 1 + max(ht p1) (ht p2)
  | Iff(p1, p2)      -> 1 + max(ht p1) (ht p2)
;;


let x = Or(T,F);;
ht x ;; (*Shoulld be 1*) 
  
        
let y = And(Or(T,F),F);;
ht y;; (*Should be 2*)

       
(*****end*****)


(**
2. size: prop -> int
*)


let rec size p =
  match p with
  | Atom _           -> 1
  | T | F            -> 1
  | Not p1           -> 1 + size(p1)
  | And (p1, p2)     -> 1 + max(size p1) (size p2)
  | Or (p1, p2)      -> 1 + max(size p1) (size p2) 
  | Imply (p1, p2)   -> 1 + max(size p1) (size p2)
  | Iff(p1, p2)      -> 1 + max(size p1) (size p2)
;;


let x = Or(T,F);;
size x ;;
let y = Imply(Or(And(T,F),T),F) ;;
size y ;; (*Should return 4*)
          
          
(*****end*****)



(*
3. atoms: prop -> string set
*) 



let rec atoms p =
  match p with
  | Atom l           -> [l]
  | T | F            -> []
  | Not p1           -> atoms p1
  | And (p1, p2)     -> atoms p1 @ atoms p2
  | Or (p1, p2)      -> atoms p1 @ atoms p2
  | Imply (p1, p2)   -> atoms p1 @ atoms p2
  | Iff(p1, p2)      -> atoms p1 @ atoms p2
;;

let at = Or(Atom "p",Or(T,Atom "q"))  ;;
atoms at;; (*Should return a ["p" ; "q"]*)

  
(*****end*****)

           
(*
4. truth: prop -> (string -> bool) -> bool,
*)

(*Defining the rho*)
let rho x = match x with 
    "w" -> true
  | "x" -> false
  | "y" -> true
  | "z" -> false
  | _   -> true
;;


let rec truth p rho =
  match p with
  | Atom s           -> rho s
  | T                -> true
  | F                -> false
  | Not p1           -> not (truth p1 rho);
  | And (p1, p2)     -> (truth p1 rho) && (truth p2 rho);
  | Or (p1, p2)      -> (truth p1 rho) || (truth p2 rho);
  | Imply (p1, p2)   -> (not (truth p1 rho)) || (truth p2 rho);  
  | Iff(p1,p2)       -> if (truth p1 rho) == (truth p2 rho) then true else false



let tr = Imply(And(Or(Atom "x" , Atom "y"),Atom "w"),F) ;;
truth tr rho;; (*Should return False as Implies(T,F) is False*)
  
  
  


(*****end*****)

(*5. nnf: prop -> prop*)

(*
   In mathematical logic, a formula is in negation normal form (NNF) 
if the negation operator (Not) is only applied to variables and the only other 
allowed Boolean operators are conjunction (And) and disjunction (Or).


*)

let rec nnf p = match p with
    T -> T
  | F -> F
  | Atom s -> Atom s

  | And (p1, p2) -> And (nnf p1, nnf p2)
  | Or (p1, p2) -> Or (nnf p1, nnf p2) 
  | Imply (p1, p2) -> Or (nnf (Not p1), nnf p2)
  | Iff (p1, p2) -> And (Or (nnf (Not p1), nnf p2), Or (nnf p1, nnf (Not p2)))
  | Not p1 -> (match p1 with
        T -> F
      | F -> T
      | Atom s -> Not (Atom s)
      | Not p2 -> nnf p2
      | And (p2, p3) -> Or(nnf (Not p2), nnf (Not p3))
      | Or (p2, p3) -> And(nnf (Not p2), nnf (Not p3))
      | Imply (p2, p3) -> And ((nnf p2), nnf (Not p3))
      | Iff(p2, p3) -> Or (And ((nnf p2), nnf (Not p3)), And (nnf (Not p2), (nnf p3))))
;;

let test = Not(Or(F,F));;
nnf test;; (*In this example Not is pushed inwards and converted into And (Not F, Not F) 
           ie. And (T,T)*) 
           
let test2 = Not(Imply(T,F));;
nnf test2;; (*Since Imply (T,F) is always F and then Not F is Truw 
            and equvalent to And(T,T)*)

let test3 = Not(Iff(T,F));;
nnf test3 ;;

let test4 = And (Not (Or (Atom "a", Atom "b")), Atom "c");;
nnf test4

  

(*****end*****)


(*6. cnf: prop -> prop set set*)

(*Product of Sum*)



let rec cnf p =
  let p1 = nnf p in
  match p1 with
    T -> T
  | F -> F
  | Atom s -> Atom s
  | Not p1 -> Not p1
  | And (p1, p2) -> And (cnf p1, cnf p2)
  | Or (p1, p2) -> (match (p1, p2) with
        (x, And (y, z)) -> And (cnf (Or (x, y)), cnf (Or (x, z)))
      | (And (x, y), z) -> And (cnf (Or (x, z)), cnf (Or (y, z)))
      | (x, y) -> Or (cnf x, cnf y))
  |_ -> p1



let test = Not(Or(F,F));;
cnf test;;
 
let test2 = Or(F, And(T,T)) ;;
cnf test2 ;;

let test3 = And(Or(T,F),F);;
cnf test3 ;;


(*****end*****)

(*7. truthcnf: prop set set -> (string -> bool) -> bool*)


(*CNF = Product of sums ie (a+b)(c+d)*)

let rec cnf_del_or p = match p with
    Or(p1,p2) -> (cnf_del_or p1)@(cnf_del_or p2)
  |q -> [q] ;;
  

let rec cnf_del_and p = match p with
    And(p1,p2) -> (cnf_del_and p1)@(cnf_del_and p2)
  |q -> [cnf_del_or q] ;;



cnf_del_or test3 ;;
cnf_del_and test3 ;; (* converting test3 into this form [[T; F]; [F]]*)


(*Converting cnf to prop set set
val cnf_pro : prop -> prop list list = <fun>
*)

let cnf_pro p  = (cnf_del_and (cnf p));;

cnf_pro F;;

let rec clist p rho = match p with
    []->false
  |[Atom a]->rho a
  |[T]->true
  |[F]->false
  |[Not p]->let pa = Not p in truth pa rho
  |y::ys -> truth y rho || clist ys rho;;


let rec truthcnf p rho = match p with
    [[]]|[]->true
 
  |[[Atom a]]->rho a
  |[[T]]->true
  |[[F]]->false
  |[[Not p]]->let pa = Not p in truth pa rho; 
  |y::ys -> clist y rho && truthcnf ys rho
;;

let test_ex = Not(And(Iff( Atom("w"), Atom("w") ), And( Not(Atom("x")), Atom("y") )));; 
let pt= cnf_pro test_ex;;
truthcnf pt rho;; 


let test_ex2 = Or(Not(Or(Iff( Atom("w"), Atom("x") ), And( Not(Atom("a")), Atom("z") ))),F);; 
let pt= cnf_pro test_ex;;
truthcnf pt rho;; 



(*****end*****)


(*8. dnf: prop -> prop set set*)

(* Sum  of Product*)


let rec dnf p =
  let p1 = nnf p in
  match p1 with
    T -> T
  | F -> F
  | Atom s -> Atom s
  | Not p1 -> Not p1
  | Or (p1, p2) -> Or (dnf p1, dnf p2)
  | And (p1, p2) -> (match (p1, p2) with
        (x, Or (y, z)) -> Or (dnf (And (x, y)), dnf (And (x, z)))
      | (Or (x, y), z) -> Or (dnf (And (x, z)), dnf (And (y, z)))
      | (x, y) -> And (dnf x, dnf y))
  |_ -> p1



let test = Not(Or(F,F));;
dnf test;;
 
let test2 = Or(F, And(T,T)) ;;
dnf test2 ;;

let test3 = And(Or(T,F),F);;
dnf test3 ;;



(*9. truthdnf: prop set set -> (string -> bool) -> bool, 
  which evaluates a given proposition in DNF, with respect to a truth assignment.
*)





(*DNF =  Sum Of Product ie (ab)+(cd)*)

let rec dnf_del_and p = match p with
    And(p1,p2) -> (dnf_del_and p1)@(dnf_del_and p2)
  |q -> [q] ;;
  

let rec dnf_del_or p = match p with
    Or(p1,p2) -> (dnf_del_or p1)@(dnf_del_or p2)
  |q -> [dnf_del_and q] ;;



dnf_del_or test3 ;;
dnf_del_and test3 ;; (* converting test3 into this form [[T; F]; [F]]*)


(*Converting dnf to prop set set
val dnf_pro : prop -> prop list list = <fun>
*)

let dnf_pro p  = (dnf_del_or (dnf p));;

dnf_pro F;;

let rec dlist p rho = match p with
    []->false
  |[Atom a]->rho a
  |[T]->true
  |[F]->false
  |[Not p]->let pa = Not p in truth pa rho
  |y::ys -> truth y rho && dlist ys rho;;


let rec truthdnf p rho = match p with
    [[]]|[]->true
 
  |[[Atom a]]->rho a
  |[[T]]->true
  |[[F]]->false
  |[[Not p]]->let pa = Not p in truth pa rho; 
  |y::ys -> clist y rho || truthcnf ys rho
;;


let test_ex = Not(And(Iff( Atom("w"), Atom("w") ), And( Not(Atom("x")), Atom("y") )));; 
let pt = dnf_pro test_ex;;
truthdnf pt rho;; 


let test_ex2 = Or(Not(Or(Iff( Atom("w"), Atom("x") ), And( Not(Atom("a")), Atom("z") ))),F);; 
let pt= dnf_pro test_ex;;
truthdnf pt rho;; 















