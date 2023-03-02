(*******************************************************************************
Midterm Exam
============
CSCI 499, Fall 2022: An Introduction to Functional Programming
Mukund Raghothaman
Due: ____________
*******************************************************************************)

(*******************************************************************************
- Your name: __________
*******************************************************************************)

(*******************************************************************************
Instructions
------------
1. Unlike the homework assignments, collaboration is disallowed on this exam.
2. Please setup your programming environment by following the steps outlined in
   the cheatsheet and on the course website.
3. After filling your reponses, rename this file to "Midterm-Yourname.ml".
4. IMPORTANT: Run #use "Midterm-Yourname.ml" in utop and make sure that you can
   cleanly import this file into utop.
5. Submit your responses using Blackboard.
*******************************************************************************)

exception NotImplemented

(** Distribution of points: 5 + 5 + 5 + 5 + 10 + 10 + 5 = 45 points *)

(******************************************************************************)
(* Question 1: Types 1 (5 points)

   State the types of each of the following expressions. Justify each of your
   answers. *)

let e1 = fun l -> (List.length l) < List.hd l

let e2 = fun f l -> (f (List.hd l)) :: l

let e3 = fun o f ->
  match o with
  | Some x -> (match f x with
               | Some v -> Some v
               | None -> None)
  | None -> None

let e4 = fun l -> List.fold_left (@) [] l

let e5 = fun f o1 o2 ->
  match o1, o2 with
  | Some x1, Some x2 -> Some (f x1 x2)
  | None, _ -> None
  | _, None -> None

(******************************************************************************)
(* Question 2: Types 2 (5 points)

   Provide examples of expressions with each of the following types: *)

let e6 : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c =
  raise NotImplemented

let e7 : 'a list list -> 'a list =
  raise NotImplemented

let e8 : 'a list -> 'a list list =
  raise NotImplemented

type ('a, 'b) either = Left of 'a | Right of 'b

let e9 : ('a * 'b) -> ('a, 'b) either =
  raise NotImplemented

let e10 : ('a, 'b) either -> ('a -> 'b) -> 'b =
  raise NotImplemented

(******************************************************************************)
(* Question 3: Types 3 (5 points)

   Each of the following expressions is ill-typed. Can you explain why? Can you
   modify them so that they are well-typed? *)

(* let e11 = 2 * 3.14 *)

(* let e12 = fun x -> x + Int.to_float x *)

(* let e13 = fun l -> l :: List.hd l *)

(* let e14 = fun f -> f f *)

(* let e15 = fun f x -> f x + f [ x ] *)

(******************************************************************************)
(* Question 4: Understanding the Mechanics of Evaluation (5 points)

   Predict the result of evaluating each of the following expressions. Justify
   each of your predictions with a sentence or two. *)

let e16 = let x = 3 in let x = 5 in x + x

let e17 = let x = 3 in (let x = 5 in x) + x

let e18 =
  let f x = let g y = x + y in g in
  let g1 = f 3 in
  let g2 = f 8 in
  (g1 6) + (g2 9)

let e19 =
  let x = ref 3 in
  let f y = (!x) + y in
  let z = f 9 in
  x := !x + 2;
  z + f 2

let e20 =
  let f x =
    let p = ref x in
    let g y =
      p := !p + y;
      !p in
    g in

  let g1 = f 3 in
  let g2 = f 9 in

  let w1 = g1 2 in
  let w2 = g2 5 in
  let w3 = g1 3 in
  let w4 = g2 6 in

  w1 + w2 + w3 + w4

(******************************************************************************)
(* Question 5: Factoring Numbers (10 points)

   Write a program that determines all prime factors of a number, with
   multiplicities. Ensure that the output is presented in sorted order. We
   present a few test cases to clarify this question and to help test your
   solutions.

   How does your solution work? *)

let (* rec? *) factors (n : int) : int list =
  raise NotImplemented

let _ =
  assert (factors 1 = []);
  assert (factors 2 = [ 2 ]);
  assert (factors 3 = [ 3 ]);
  assert (factors 4 = [ 2; 2 ]);
  assert (factors 5 = [ 5 ]);
  assert (factors 6 = [ 2; 3 ]);
  assert (factors 24 = [ 2; 2; 2; 3 ])

(* The only known polynomial time solutions to this problem require quantum
   computers. Whether a classical polynomial time algorithm exists is one of the
   most important open problems in computer science. *)

(******************************************************************************)
(* Question 6: Polynomial Differentiation (10 points)

   We can represent a single variable polynomial

     f(x) = a_0 x ** 0 + a_1 x ** 1 + a_2 x ** 2 + ... + a_n x ** n

  as the list of its coefficients, lf = [a_0; a_1; a_2; ...; a_n]. We also agree
  that the highest coefficient explicitly mentioned in this list must satisfy
  a_n > 0.

  Write a function to compute the derivative d/dx f(x) of a polynomial
  represented in this manner. Recall that:

     d/dx f(x) = (1 * a_1) x ** 0 + (2 * a_2) x ** 1 + (3 * a_3) x ** 2 + ... +
                 (n * a_n) x ** (n - 1)

  so that

     derivative lf = [1 * a_1; 2 * a_2; 3 * a_3; ...; n * a_n] *)

let derivative (lf : float list) : float list =
  raise NotImplemented

(* We provide some test cases to help you verify your solution: *)

let _ =
   assert (derivative [1.; 2.; 1.; 4.] = [2.; 2.; 12.]);
   assert (derivative [] = []);
   assert (derivative [1.] = [])

(******************************************************************************)
(* Question 7: Maps, Filters, and Folds (5 points)

   In this question, we continue our study of the connections between the
   various list processing functions. Recall the definition of map and
   fold_right: *)

let rec map (f : 'a -> 'b) (l : 'a list) : 'b list =
  match l with
  | [] -> []
  | hd :: tl -> (f hd) :: (map f tl)

let rec fold_right (f : 'a -> 'b -> 'b) (l : 'a list) (acc : 'b) : 'b =
  match l with
  | [] -> acc
  | hd :: tl -> f hd (fold_right f tl acc)

(* Present an alternative implementation of map using fold_right as a
   primitive: *)

let map2 (f : 'a -> 'b) (l : 'a list) : 'b list =
  let g = raise NotImplemented in
  let acc = raise NotImplemented in
  fold_right g l acc

(* Next, recall the definition of filter: *)

let rec filter (f : 'a -> bool) (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | hd :: tl -> if f hd then hd :: (filter f tl) else filter f tl

(* Present an equivalent implementation, once again using fold_right as a
   primitive: *)

let filter2 (f : 'a -> bool) (l : 'a list) : 'a list =
  let g = raise NotImplemented in
  let acc = raise NotImplemented in
  fold_right g l acc
