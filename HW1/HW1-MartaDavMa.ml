(*******************************************************************************
Homework 1
==========
CSCI 499, Fall 2022: An Introduction to Functional Programming
Mukund Raghothaman
Due: 10pm PT on 16 September, 2022
*******************************************************************************)

(*******************************************************************************
- Your name: Marta Davila Mateu
- Your partner's name (if any): __________
*******************************************************************************)

(*******************************************************************************
Instructions
------------
1. Please setup your programming environment by following the steps outlined in
   the cheatsheet and on the course website.
2. Please feel free to collaborate with a partner, but make sure to write your
   submission individually.
3. Rename this file to HW1-yourname.ml, where yourname is your name, before
   submitting it on Blackboard.
4. Make sure that I can cleanly import this file into the OCaml toplevel when I
   call #use "HW1-yourname.ml". Comment out any questions you were unable to
   solve / have left unanswered.
*******************************************************************************)

exception NotImplemented

(** Distribution of points: 5 + 6 + 6 + 13 + 10 + 10 + 10 + 10 = 75 *)

(******************************************************************************)
(* Question 1: Starting Off (5 points)
   State the types and the results of evaluating the following expressions. *)

let _ = 5 + 3 - 2;;
(* - : int = 6 *)
let _ = 2. ** 0.5;;
(* - : float = 1.41421356237309515 *)
let _ = [1; 2; 3] @ [4; 5; 6];;
(* - : int list = [1; 2; 3; 4; 5; 6] *)
let _ = ("abc" ^ "def").[3];;
(* - : char = 'd' *)
let _ = fun x -> x + 1;;
(* - : int -> int = <fun> *)




(******************************************************************************)
(* Question 2: Simple Expressions (2 + 4 points) *)

(* 2a. Recall the formula for the solutions of a quadratic equation
       a * x ** 2 + b * x + c = 0: (-b +/- sqrt(b ** 2 - 4 * a * c)) / (2 * a).
       The roots are real-valued iff the discriminant b ** 2 - 4 * a * c is
       non-negative.

       Write a function solveQuad a b c : (float * float) option which returns
       the real-valued roots of a quadratic equation, if they exist.

       In the following declaration, replace the call to raise with your
       implementation. *)

let solveQuad (a : float) (b : float) (c : float) : (float * float) option =
       let toSqr = ((b *. b) -. (4. *. a *. c)) in
       if toSqr < 0. then
           None
       else
           let sol1 = ((0. -. b +. sqrt toSqr) /. (2. *. a)) in
           let sol2 = ((0. -. b -. sqrt toSqr) /. (2. *. a)) in
           Some (sol1, sol2)


(* 2b. For a given initial positive integer value n1, the Collatz sequence
       n1, n2, n3, ... is defined as follows:
       n{i + 1} = ni / 2, if ni is even, and
                  3 * ni + 1, otherwise.
       For example, for n1 = 3, the sequence is 3, 10, 5, 16, 8, 4, 2, 1, 4, 2,
                                                1, 4, 2, 1, ..., and
                    for n1 = 12, the sequence is 12, 6, 3, 10, 5, 16, 8, 4, 2,
                                                 1, ...

       The famous Collatz conjecture states that regardless of the initial value
       n1, the sequence always reaches 1.

       Write a function collatzLen : int -> int such that collatzLen n1 is the
       number of elements in the Collatz sequence before attaining the value 1.
       For example, collatzLen 3 = 7, collatzLen 12 = 9, and collatzLen 1 = 0.
       For full credit, make sure that your implementation is tail-recursive.
       Explain how your solution works. *)

let collatzLen (n : int) : int = 
    let rec helper (ni : int) (cnt : int) =
        if ni = 1
            then cnt
        else
            if ni mod 2 = 0
                then helper (ni/2) (cnt + 1)
            else
                helper (3 * ni + 1) (cnt + 1)
    in helper n 0;;

      (* This function avoids waisting memory by reusing the same moemory 
      location in each execution of the recursion. To do so it uses the 
      "helper" function, which has a counter itself, and does not need to 
      keep anything in the memory for previos executions, as every action
      within the helper function is done at each execution. *)
            



(******************************************************************************)
(* Question 3: Evaluation Sequences (2 + 4 points)

   Recall the evaluation sequence for the factorial function, *)

let rec factorial n = if n = 0 then 1 else n * factorial (n - 1)

(* factorial 4 ==> if 4 = 0 then 1 else 4 * factorial (4 - 1)
               ==> if false then 1 else 4 * factorial (4 - 1)
               ==> 4 * factorial (4 - 1)
               ==> 4 * factorial 3
               ==> 4 * (if 3 = 0 then 1 else 3 * factorial (3 - 1))
               ==> 4 * (if false then 1 else 3 * factorial (3 - 1))
               ==> 4 * (3 * factorial (3 - 1))
               ==> 4 * (3 * factorial 2)
               ==> ...
               ==> 4 * (3 * (2 * (1 * 1)))
               ==> 4 * (3 * (2 * 1))
               ==> 4 * (3 * 2)
               ==> 4 * 6
               ==> 24

   Define the naive function to compute Fibonacci numbers as follows: *)

let rec fib n =
   if n = 0 then 0
   else if n = 1 then 1
   else fib (n - 1) + fib (n - 2)

(* 3a. State the evaluation sequence of fib 3.

 fib 3 ==> if 3 = 0 then 0 else if 3 = 1 then 1 else fib (3 - 1) + fib (3 - 2)
       ==> if false else false else fib 2 + fib 1
       ==> fib 2 + fib 1
       ==> (f 2 = 0 then 0 else if 2 = 1 then 1 else fib (2 - 1) + fib (2 - 2)) +
           (f 1 = 0 then 0 else if 1 = 1 then 1 else fib (3 - 1) + fib (3 - 2))
       ==> (false else false else fib 1 + fib 0) + (false else true 1)
       ==> ((false else true 1) + (true 0)) + (1)
       ==> 1 + 0 + 1
       ==> 2


   3b. If a function is tail recursive, then the size the intermediate
       expressions in the evaluation sequence is bounded by a constant. Write an
       alternative tail-recursive function fib2 : int -> int to compute
       Fibonacci numbers. Explain how your implementation works. *)

let fib2 (n : int) : int =
   if n = 0 then 0
   else if n = 1 then 1
   else 
      let rec helper (nm1 : int) (nm2 : int) (cnt : int): int =
         if cnt = n then (nm1 + nm2)
         else 
            helper (nm1 + nm2) (nm1) (cnt + 1)
      in helper 0 1 2

      (* Just like the collatzLen function, this function function avoids 
      waisting memory by reusing the same moemory location in each execution 
      of the recursion. To do so it uses the "helper" function, which has a 
      counter itself, and does not need to keep anything in the memory from 
      previos executions. To compute the Fibonacci number only the last two 
      Fibonacci numbers are needed. So what we are doing is always calling the
      helper function with these last two already computer Fibonacci numbers.
      Then, when the counter is done, we simply return the sum of these two 
      computed Fibonacci numbers. Otherwise we call the helper again with 
      the previous Fibonacci number (not both) and the sum of both. *)

  

(******************************************************************************)
(* Question 4: Types (5 + 8 points)

   4a. Provide examples of expressions with the following types:
       - int *)
       let a = 3;;

       (* - int list *)
       let b = [1; 2; 3];;

       (* - int list -> int *)
       let rec length (l : int list) : int =
          match l with
            | [] -> 0
            | _ :: t -> 1 + length t;;

       (* - 'a -> ('a -> 'b) -> 'b *)
       let func param (f) = 
          f (param);;

       (* - 'a -> ('b -> 'a) -> 'b *)
      let rec magic x = magic x

       let f a g =
         let b = magic g in
         if g b = a then g
         else g
       
       (* Returns 'a -> ('b -> 'a) -> 'b -> 'a, but I don't know how to get b to 
         not imply a *)

   (* 4b. Provide examples of expressions e so that the following expressions are
       well-typed:
       - let e = 1 in 3 + e
       - let e = int_of_float in 3 + (e 3.2)
       - let e = length in (e [1; 2; 3]) + 2
       - let e = fun x -> 1 in (fun x -> x 0) e
       *)  



(*******************************************************************************)
(* Question 5: Higher-Order Functions (4 + 6 points)

   5a. The integral of a function f between two points a and b is defined as
       (f a + f (a + epsilon) + f (a + 2 * epsilon) + ... + f (a + k * epsilon))
       * epsilon, where epsilon > 0 is a small real number, and k is the largest
       integer such that a + k * epsilon <= b.

       Write an expression (integral f a b epsilon) which computes the integral
       of f between a and b with interval size epsilon. State its type. Explain
       how your implementation works. *)

let integral f a b epsilon =
   let rec helper k result = 
       if k <= ((b - a) / epsilon) then result
       else
          helper (k + 1) (result + f(a + (k * epsilon)))
   in helper 1 (f(a));;

   (* This function is of type  (int -> int) -> int -> int -> int -> int.
   Again, this function uses a helper to be able to do tail-recursion. It 
   works by checking on each execution of the helper if the condition to stop
   (a + k * epsilon <= b) is acheived. If it is not, it calls the helper adding
   the computed result, and adding one to the counter k. *)


(* 5b. Consider the following expressions doTwo, doThree, doFour, etc. which
       take a function f and apply it two, three, four, etc. times to the input
       argument x: *)

let doTwo f x = f (f x)
let doThree f x = f (f (f x))
let doFour f x = f (f (f (f x)))

(*  - Write a function doN such that (doN n) : ('a -> 'a) -> 'a -> 'a is the
      corresponding function which does something n times. Explain how your
      implementation works. *)

      let rec doN (n : int) =
         let rec help (cnt : int ) =
            fun f x -> 
            if cnt = 0 then x
            else help (cnt-1) f (f x)
         in help n

(* doN is a function that given an integer n, executes f of x (do something with x), 
n times. A helper function is in charge of the recursion by keeping truck of the
executions left. If cnt is 0 it means that "something" has been done n times and
the program can return X. Otherwise another execution is needed, discounting 1 from cnt.*)

(*    To test your solution, you might find the following function useful: *)

let explain n = n Int.succ 0

(*    The function Int.succ : int -> int returns the successor of its input
      argument. For example, Int.succ 3 evaluates to 4, and Int.succ 5 evaluates
      to 6. You will notice that (explain doTwo) evaluates to 2,
      (explain doThree) evaluates to 3, and so on. If your submission is
      correct, (explain (doN n)) will evaluate to n, for all integer values n.

    - Explain, in English, how the function named explain works.
       The function explain defines a f and a x (as called in our function doN). 
       For f it defines the function Int.succ, which simply evaluates with the 
       following integer. For x it defines the starting point 0.

    - Write a function add which adds two numbers represented in this manner. In
      particular, for all machine integers m and n, ensure that
      explain (add (doN m) (doN n)) evaluates to (m + n). *)

let add m n = 
   let m2 = explain (doN m)
in
   let n2 = explain (doN n)
in m2 + n2



(******************************************************************************)
(* Question 6: One Nontrivial Algorithm (10 points)

   A binary search tree is defined as follows: *)

type tree = Leaf
          | Node of int * tree * tree
          [@@deriving show]

(* with the invariant that all numbers in the left subtree are strictly less
   than the value at the root, and all numbers in the right subtree are strictly
   greater than the value at the root: *)

let is_bst t =
  let rec bounds l t h = match t with
                         | Leaf -> true
                         | Node(n, t1, t2) -> (l < n) &&
                                              bounds l t1 n && bounds n t2 h &&
                                              (n < h)
  in bounds min_int t max_int

(* Write a function merge : tree -> tree -> tree which merges two binary search
   trees into a single tree containing all numbers in either of the input trees.
   Explain how your solution works. *)

let merge (t1 : tree) (t2 : tree) : tree =
   
   let rec inorder (t : tree) : int list =
      match t with
      | Leaf -> []
      | Node (c, tl, tr) -> (inorder tl) @ [c] @ (inorder tr) in

   let l1 = inorder t2 in
   let l2 = inorder t2 in

   let rec merge (l1 : int list) (l2 : int list) : int list =
      match l1, l2 with
      | [], _ -> l2
      | _, [] -> l1
      | hd1 :: tl1, hd2 :: tl2 -> if hd1 <= hd2 then hd1 :: (merge tl1 l2)
                                                else hd2 :: (merge l1 tl2) in

   let l = merge l1 l2 in

   let rec buildTree (l : int list) : tree =
     match l with
     | [] -> Leaf 
     | hd :: tl -> Node(hd, Leaf, buildTree tl)
   in buildTree l



(******************************************************************************)
(* Question 7: Expression Evaluator (4 + 6 points)

   In this question, we will implement our first interpreters for an extremely
   simple language of arithmetic expressions. Consider the type expr1 of
   arithmetic expressions defined as follows: *)

type expr1 = Int1 of int
           | Plus1 of expr1 * expr1
           | Minus1 of expr1 * expr1
           | Mult1 of expr1 * expr1
           [@@deriving show]

(* For example, the value Plus1(Int1 3, Int1 8) represents the expression
   (3 + 8) which, when evaluated, produces the result 11. The base constructor
   Int1 constructs an expression out of an integer value, while the Plus1,
   Minus1 and Mult1 constructors encode the addition, subtraction and
   multiplication operations respectively.

   7a. Write a function eval1 : expr1 -> int which produces the result of
       evaluating an expression. *)

let eval1 (e : expr1) : int = 
  match e with
  | Int1 (a) -> a
  | Plus1 (a, b)-> 
         (match a with | Int1 (c) -> c | Plus1(a, b) | Minus1(a, b) | Mult1(a, b) -> 0 ) + 
         (match b with | Int1 (c) -> c | Plus1(a, b) | Minus1(a, b) | Mult1(a, b) -> 0 )
  | Minus1 (a, b)-> 
         (match a with | Int1 (c) -> c | Plus1(a, b) | Minus1(a, b) | Mult1(a, b) -> 0 ) - 
         (match b with | Int1 (c) -> c | Plus1(a, b) | Minus1(a, b) | Mult1(a, b) -> 0 )
  | Mult1 (a, b)-> 
         (match a with | Int1 (c) -> c | Plus1(a, b) | Minus1(a, b) | Mult1(a, b) -> 0 ) * 
         (match b with | Int1 (c) -> c | Plus1(a, b) | Minus1(a, b) | Mult1(a, b) -> 0 )

(*     Next, we extend the type expr1 of expressions with constructors that
       encode variables and let expressions. The following type expr2 is just
       like expr1, except that the expression
       (Let2 "x" (Int2 3) (Plus2(Var2 "x", Var2 "x"))) stands for the OCaml
       expression (let x = 3 in x + x). As in OCaml, variables are statically
       scoped. *)

type expr2 = Int2 of int
           | Plus2 of expr2 * expr2
           | Minus2 of expr2 * expr2
           | Mult2 of expr2 * expr2
           | Var2 of string
           | Let2 of string * expr2 * expr2
           [@@deriving show]


(* 7b. Write a function (eval2 : expr2 -> int option) which returns the result
       of evaluating an expression, if it is defined.

       You will find it useful to maintain a map from variable names to bound
       values. Recall how maps work from our discussion in class on Aug 31. We
       invoke the following magic incantation to start you off: *)

module StringMap = Map.Make(String)
let empty_map = StringMap.empty
       
(*     To bind a key k to a value v, write (StringMap.add k v m). For
      example: *)

(* let m1 = StringMap.add "x" 3 empty_map *)

(*     To check if a map m contains a key k, write (StringMap.find_opt k m). For
      example, (StringMap.find_opt "x" m1) evaluates to Some 3, and
      (StringMap.find_opt "y" m1) evaluates to None.

      Note that maps are immutable objects: the result of evaluating
      StringMap.add is a new map with the desired key-value association; the
      original map is unaltered. For example,
      (StringMap.find_opt "x" empty_map) still evaluates to None. *)


let eval2 (e : expr2) : int option = 
   let map = ref empty_map
   in

   let findInt (f : expr2) : int =
      match f with
      | Var2 (a)-> (int_of_float(float(StringMap.find a !map)))
      | Int2 (a)  -> (a)
      | Plus2 (a,b)  -> 0
      | Minus2 (a,b)  -> 0
      | Mult2 (a,b)  -> 0
      | Let2 (a,b,c) -> 0
   in

   let addNew (n : string) (v : expr2) =
       map := StringMap.add n (findInt v) !map
   in

   let rec matcher (g : expr2) : int option = 
      match g with
      | Int2 (a) -> Some(a)
      | Var2 (a)-> Some(int_of_float(float(StringMap.find a !map)))
      | Plus2 (a,b) -> Some(findInt(a) + findInt(b))
      | Minus2 (a,b) -> Some(findInt(a) - findInt(b))
      | Mult2 (a,b) -> Some(findInt(a) * findInt(b))
      | Let2 (a,b,c) -> (addNew a b); matcher c

   in matcher e



(******************************************************************************)
(* Question 8: Tail Recursive In-Order Traversals (Challenge Problem, 15 points)

   Recall the traditional approach to perform an in-order traversal of a tree
   t: *)

let rec inorder (t : tree) : int list =
  match t with
  | Leaf -> []
  | Node(a, tl, tr) -> (inorder tl) @ [ a ] @ (inorder tr)

(* Observe that inorder is not tail-recursive because it makes two recursive
   calls to itself. Implement an equivalent tail-recursive version
   inorder2 : tree -> int list. Explain how your solution works.

   This question is significantly harder than Questions 2b and 3b, but captures
   the essence of why _any_ computation can be expressed using tail recursive
   calls. *)

(* keep a to do list with what is left in the back *)
(*
INTUITION:
1. -- tree left
   1.1 -- tree left
      1.1.1 tree left
      1.1.2 add to list
      1.1.3 tree right
   1.2 add to list
   1.3 tree right
2. add to list
3. tree right
*)
let inorder2 (t : tree) : int list =

   (* keep list of:
      2. tree list = keeps trees to do
      3. int list = keeps int to add
      4. int list = current output
      *)
   let rec helper (n : tree) (trees : tree list) (ints : int list)
   (output : int list) : int list = 
   match n with
   | Leaf -> (* do next thing in list *)
      (match ints with (* add first element in ints to output and remove from ints *)
      | [] -> 
         (match trees with
         | [] -> output (* both empty, all todos done*)
         | nt :: rest -> helper nt rest ints output)
      | a :: rest2 ->  
         (match trees with
         | [] -> helper n trees rest2 (output @ [a])
         | nt :: left -> helper nt left rest2 (output @ [a])))
   | Node(c, tl, tr) -> (helper tl ([tr] @ trees) ([c] @ ints) output)

   in (helper t [] [] [])





(* OPTION TWO: always go right until left
   
NOT IMPLEMENTED

Pseudo code:
order = empty list 

rec findLowestStart(node)
if node.left = empty
   return node;
else
   findLowestStart(node.left)


rec findNext(node)
if node.right != empty
   next = findLowestStart(node.right)
   order.add(next)
   findNext(next)
else if node.parent.left = node
   next = node.parent
   order.add(next)
   findNext(next)
else if node.parent.right = node
   next =findTopStart(node.parent.right)
   order.add(next)
   if next.right != empty
      order.add(next.right)
      findNext(next.right)
   else
      next2 = findTopStart(next)
      order.add(next2)
      findNext(next2)
else
   return order (not needed)


rec findTopStart(node)
   if node.parent.right = node
      findTopStart(node.parent.right)
   else if node.parent.left = node
      order.add(node.parent)
      if node.parent.right != empty
         next = findLowestStart(node.parent.right)
         order.add(next)
         findNext(next)
      else
         findTopStart(node.parent)
*)
