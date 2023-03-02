(*******************************************************************************
Final Exam
==========
CSCI 499, Fall 2022: An Introduction to Functional Programming
Mukund Raghothaman
Due: 10pm PT on December 12, 2022
*******************************************************************************)

(*******************************************************************************
- Your name: __________
*******************************************************************************)

(*******************************************************************************
Instructions
------------
1. Please setup your programming environment by following the steps outlined in
   the cheatsheet and on the course website.

2. Unlike the homework assignments, collaboration is disallowed on this exam.

3. As part of your submission, rename this file to "Final-Yourname.ml". Make
   sure that I can cleanly import this file into the Ocaml toplevel when I call
   #use "main.ml". Comment out any questions you were unable to solve / have
   left unanswered, and submit using Blackboard.
*******************************************************************************)

(** Distribution of points: 10 + 20 + 10 + 10 + 10 = 60 points *)

(******************************************************************************)
(* 0: Preamble

   We define some things that you might find helpful in your solutions: *)

exception NotImplemented

module Pmap =
  struct
    open Base

    (* You may represent a map / dictionary from keys of type 'a to values of
       type 'b using the type ('a, 'b) Pmap.t. *)
    type ('a, 'b) t = ('a, 'b) Map.Poly.t

    (* Pmap.empty is the empty map, and Pmap.is_empty m determines whether a map
       m is empty. *)
    let empty : ('a, 'b) t = Map.Poly.empty
    let is_empty : ('a, 'b) t -> bool = Map.Poly.is_empty

   (* Pmap.add k v m constructs a new map in which the key k is mapped to the
      value v. Any previous k -> v' mapping is overwritten, if it exists.
      Pmap.find k m returns the value associated with key k in map m, and None
      if the association cannot be found. *)
    let add (key : 'a) (value : 'b) (m : ('a, 'b) t) : ('a, 'b) t =
      Map.Poly.set m ~key:key ~data:value
    let find (key : 'a) (m : ('a, 'b) t) : 'b option =
      Map.Poly.find m key

    (* Pmap.of_alist_multi and Pmap.to_alist are functions that can convert maps
       to and from association lists respectively. Pmap.map is the standard
       counterpart of the map function for dictionaries. *)
    let of_alist_multi (l : ('a * 'b) list) : ('a, 'b list) t =
      Map.Poly.of_alist_multi l
    let to_alist (m : ('a, 'b) t) : ('a * 'b) list =
      Map.Poly.to_alist ~key_order:`Increasing m
    let map (f : 'b -> 'c) (m : ('a, 'b) t) : ('a, 'c) t =
      Map.Poly.map m ~f:f
  end

module Pset =
  struct
    open Base

    (* You may represent a set of objects of type 'a using values of type
       'a Pset.t. *)
    type 'a t = 'a Set.Poly.t

    (* Pset.empty constructs the empty set, and Pset.is_empty s tests whether a
       set s is empty. Pset.is_subset s1 s2 determines whether the set s1 is a
       subset of s2. *)
    let empty : 'a t = Set.Poly.empty
    let is_empty : 'a t -> bool = Set.Poly.is_empty
    let is_subset (s1 : 'a t) (s2 : 'a t) : bool = Set.Poly.is_subset s1 ~of_:s2

    (* Pset.add v s constructs the new set which may be mathematically
       represented as { v } union s. Pset.contains s v tests whether the set s
       contains the value v. *)
    let add : 'a t -> 'a -> 'a t = Set.Poly.add
    let contains : 'a t -> 'a -> bool = Set.Poly.mem

    (* Pset.of_list and Pset.to_list are utility functions which can convert
       sets to and from lists respectively. *)
    let of_list : 'a list -> 'a t = Set.Poly.of_list
    let to_list : 'a t -> 'a list = Set.Poly.to_list
  end

(******************************************************************************)
(* Question 1: Timing the Bitcoin Bubble (10 points)

   Your instructor first heard about Bitcoin in early 2010, about one year after
   its inception, and long before its dramatic rise in value (and its subsequent
   ongoing crash). He was, unfortunately, a penniless undergrad at the time.
   Months later, rather than do the sensible thing and join the industry, he
   went off to graduate school, committing himself to a lifetime of asceticism.
   Still, he often wistfully looks at the historical graph of the Bitcoin--USD
   exchange rate.

   Given an initial investment and a list of historical prices, calculate the
   most he would be worth today, if he had bought and sold Bitcoins at the very
   best moments in time. For example:

     best_plan 100 [5; 18; 42; 36] = 840.

   Here, he would purchase 20 Bitcoins on the first day, and sell them on the
   third, just before the drop in prices. As a second example, observe that:

     best_plan 300 [6; 18; 64; 32; 16; 96; 104] = 20800.

   He would buy 300 / 6 = 50 Bitcoins on the first day, and sell them all on the
   third, thus obtaining 50 * 64 = 3200 USD. He would stay away from the
   investment on the fourth day, and reinvest all his savings on the fifth, to
   purchase 3200 / 16 = 200 Bitcoins. He would then sell them all on the last
   day, with a final net worth of 200 * 104 = 20800 USD. Assume that one may
   only buy or sell a positive integral number of Bitcoins at each step, so
   that:

     best_plan 100 [30; 40; 50; 45; 40; 80; 100] = 400.

   For full-credit, your solution should be tail-recursive. You may assume that
   the division operation, (a / b), returns the integer portion of the result of
   dividing a by b, and that the modulo operation, (a % b), returns the
   remainder resulting from dividing a by b. *)

let best_plan (initial_investment : int) (history : int list) : int =
  raise NotImplemented

(******************************************************************************)
(* Question 2: Simple Compiler Optimizations (10 + 10 = 20 points)

   Recall the abstract syntax of arithmetic expressions we have repeatedly
   referred to in this course: *)

type expr1 =
  | Float1 of float
  | Plus1 of expr1 * expr1
  | Minus1 of expr1 * expr1
  | Mult1 of expr1 * expr1
  | Var1 of string
  | Let1 of string * expr1 * expr1

(* One difference is that this language operates with float as its base type
   instead of int. We will be computing partial derivatives in the next
   question, which is why we make this change, but the Int vs. Float difference
   is unimportant for this question.

   Here is its evaluation function, where env_t is the type of evaluation
   environments. An evaluation environment is a map from variable names to their
   values. *)

type env_t = (string, float) Pmap.t

let rec eval (env : env_t) (e : expr1) : float option =
  let safe_apply (f : 'a -> 'b -> 'c) (oa : 'a option) (ob : 'b option) : 'c option =
    match (oa, ob) with
    | (Some a, Some b) -> Some (f a b)
    | _ -> None in

  match e with
  | Float1 c -> Some c
  | Plus1(e1, e2) -> safe_apply (+.) (eval env e1) (eval env e2)
  | Minus1(e1, e2) -> safe_apply (-.) (eval env e1) (eval env e2)
  | Mult1(e1, e2) -> safe_apply ( *. ) (eval env e1) (eval env e2)
  | Var1 v -> Pmap.find v env
  | Let1(v, e1, e2) ->
      let ov1 = eval env e1 in
      match ov1 with
      | Some v1 -> eval (Pmap.add v v1 env) e2
      | _ -> None

(* Compiler optimizers apply a sequence of transformations that make the
   resulting code execute faster, or consume less memory. Let's use a simple
   model of execution cost: *)

let rec evalCost (e : expr1) : int =
  match e with
  | Float1 _ -> 1
  | Plus1(e1, e2) | Minus1(e1, e2) | Mult1(e1, e2) -> 1 + (evalCost e1) + (evalCost e2)
  | Var1 _ -> 1
  | Let1(x, e1, e2) -> 1 + (evalCost e1) + (evalCost e2)

(* We've assumed that each operation, including loading and storing a variable
   into the environment require 1 unit of time. For example, the execution cost
   of Plus1(Var1 "x", Var1 "y") is 3 time units.

   Now consider the expression:

     let e1 = Let1("z", Plus1(Var1 "x", Float1 3.), Plus1(Var1 "x", Var1 "y"))

   This expression may be written in more intuitive notation as
   (let z = x +. 3. in (x + y)), and has evaluation cost 7. Notice, however, that
   the value stored in z as part of the let binding is not used in subsequent
   calculations. For all environments env, it follows that e1 and e2 produce the
   same output: eval env e1 = eval env e2, where:

     let e2 = Plus1(Var1 "x", Var1 "y").

   However the new expression e2 has a lower execution cost of only 3 time
   units. Our first optimization is to automate this process of eliminating dead
   code. Write a function which finds and removes unused let bindings from an
   expression: *)

let deadCodeElim (e : expr1) : expr1 =
  raise NotImplemented

(* For all expressions e and environments env, it must be the case that
   eval env e = eval env (deadCodeElim e). Explain your code.

   Now consider the expression:

     (* let e3 = (x +. y) +. ((x +. y) +. (x +. y)) *)
     let e3 = Plus1(Plus1(Var1 "x", Var1 "y"),
                    Plus1(Plus1(Var1 "x", Var1 "y"),
                          Plus1(Var1 "x", Var1 "y"))).

   Evaluating e3 takes 11 units of time. However, observe that the expression is
   computing the value of Plus(Var "x", Var "y") in three different places,
   spending a combined total of 9 time units on the computation. However, if we
   were to compute the value once and store it in a temporary register, then we
   can save 2 cycles of computation time:

     let e4 = Let1("z", Plus1(Var1 "x", Var1 "y"),
                   Plus1(Var1 "z", Plus1(Var1 "z", Var1 "z"))).

   Notice again that for all environments env, eval env e3 = eval env e4.
   Furthermore, evalCost e3 = 11 and evalCost e4 = 9.

   Thus, the compiler can safely optimize expression e3 into expression e4. The
   transformation we have applied is called common subexpression elimination.
   Given multiple occurrences of a sub-expression, we can store the result of
   the computation in a new intermediate value and reuse this result in place of
   the original occurrences.

   Implement the common subexpression elimination transformation: *)

let commonSubexprElim (e : expr1) : expr1 =
  raise NotImplemented

(* Explain your solution. Are there situations where common subexpression
   elimination would not yield a benefit in running time?

   Note that this question is somewhat tricky. In particular, pay close
   attention to the shadowing behavior of let expressions. Understand why the
   following two expressions are inequivalent:

     let e5 = Let1("x", Int1 3, Let1("y", Int1 5,
                   Plus1(Plus1(Var1 "x", Var1 "y"),
                         Let1("y", Int1 2, Plus1(Var1 "x", Var1 "y"))))), and

     let e6 = Let1("x", Int1 3, Let1("y", Int1 5,
                   Let1("z", Plus1(Var1 "x", Var1 "y"),
                        Plus1(Var1 "z", Let1("y", Int1 2, Var1 "z"))))).

   Make sure that your solution doesn't fall into this trap! *)

(******************************************************************************)
(* Question 3: Symbolic Differentiation (10 points)

   An important driver of modern machine learning is gradient descent, or the
   ability to automatically compute partial derivatives of functions and use
   them to guide the optimization process. We will now write a simple symbolic
   differentiation routine for our expression library.

   Observe that the expressions that we can represent using the expr1 type are
   essentially simple multivariate polynomials. Recall from Calculus 101 the
   following rules for partial differentiation of polynomials:

   - d/dx (x) = x, and d/dx (y) = 0., if x != y,
   - d/dx (f +. g) = d/dx (f) +. d/dx (g),
   - d/dx (f -. g) = d/dx (f) -. d/dx (g),
   - d/dx (f *. g) = f *. d/dx (g) +. d/dx (f) *. g.

   Write a function which consumes an expression and computes its partial
   derivative with respect to a given variable.

   Pay special attention to the differentiation of let expressions! Notice that:

     d/dx (let y = x in y) = 1.,

   or some other expression equivalent to 1., such as (let yp = 1. in yp). *)

let partialDerivative (x : string) (e : expr1) : expr1 =
  raise NotImplemented

(******************************************************************************)
(* Question 4: Types and Dimensions (10 points)

   The Mars Climate Orbiter was a $330 million NASA mission to study the Martian
   climate. The probe was launched from Cape Canaveral Air Force Station on
   December 11, 1998, and reached the vicinity of Mars 286 days later on
   September 23, 1999. Mission controllers lost contact with the spacecraft as
   it passed behind the planet. Unfortunately, they never managed to reestablish
   contact. An investigation board studied the failure, and reached the
   conclusion that ground software developed by Lockheed Martin reported results
   using US customary units (pounds force seconds), while the rest of NASA's
   infrastructure assumed that it was dealing with quantities expressed in SI
   units (Newton seconds). This caused a discrepancy between the actual and
   expected position of the spacecraft, leading to its subsequent loss.

   One popular application of static types is in performing dimensionality
   analysis and tracking units of measure. In this question, we study the
   simpler problem of ensuring that computations involving physical quantities
   are dimensionally sound: it makes sense to add lengths of time (seconds) to
   lengths of time (seconds), and velocities (meters per second) to velocities
   (meters per second), but it does not make sense to add time (seconds) to
   length (meters).

   For the purposes of this question, the dimensions of a physical quantity are
   expressed as a triple of integers, (l, m, t), indicating its exponents along
   the dimensions of length, mass, and time respectively. For example, a
   quantity representing the distance between Los Angeles and New York would
   have the dimensions (1, 0, 0), while the length of time between the first and
   last CSCI 499 class would have the dimensions (0, 0, 1). Some quantities,
   such as the number of classes in the semester, are dimensionless, and would
   be represented by the triple (0, 0, 0). *)

type dimensions = int * int * int

(* We then introduce a simple language of arithmetic computations which tracks
   the dimensions of its operands: *)

type expr2 =
  | Float2 of float * dimensions
  | Plus2 of expr2 * expr2
  | Minus2 of expr2 * expr2
  | Mult2 of expr2 * expr2
  | Div2 of expr2 * expr2

(* For simplicity, we have eliminated variables and let expressions from this
   language. Write a function which computes the dimensions of an arithmetic
   expression, if they exist: *)

let rec dim2 (e : expr2) : dimensions option =
  raise NotImplemented

(* If your solution is correct, it must pass the following tests:

   let _ =
     let g = Float2(9.8, (1, 0, -2)) in
     let t = Float2(2.1, (0, 0, 1)) in
     let v = Mult2(g, t) in
     let nonsense = Plus2(Mult2(v, t), v) in

     assert (dim2 g = Some(1, 0, -2));
     assert (dim2 t = Some(0, 0, 1));
     assert (dim2 v = Some(1, 0, -1));
     assert (dim2 nonsense = None)

   Most simply, the expressions Plus2(e1, e2) and Minus(e1, e2) are
   dimensionally sound iff the dimensions of e1 equal the dimensions of e2,
   while the dimensions of Mult2(e1, e2) and Div2(e1, e2) are respectively the
   sum and difference of the dimensions of the two expressions. *)

(******************************************************************************)
(* Question 5: Understanding Datalog (4 + 6 = 10 points)

   a. Which of the following Datalog queries are well-stratified, and which are
      not? Explain your responses. You may assume reasonable schemas for all
      relations.

      - path(x, y) :- edge(x, y).
        path(x, z) :- edge(x, y), path(y, z).
        nopath(x, y) :- !path(x, y)

      - even("zero").
        even(z) :- even(x), succ(x, y), succ(y, z).
        odd(x) :- !even(x).

      - even("zero").
        even(y) :- odd(x), succ(x, y).
        odd(y) :- even(x), succ(x, y).

      - even("zero").
        odd(x) :- !even(x).
        even(y) :- odd(x), succ(x, y).

   b. The following programs contain an unnecessarily large number of rules. How
      many of them can you delete without changing the meaning of the overall
      program? Explain your responses. As before, assume reasonable schemas for
      all relations.

      - path(x, y) :- edge(x, y).
        path(x, z) :- edge(x, y), path(y, z).
        path(x, z) :- path(x, y), edge(y, z).

      - step(x, y) :- edge(x, y).
        step(x, y) :- step(x, y), step(x, u).

      - sub(x, y) :- R(x, y).
        sub(x, z) :- sub(x, y), sub(y, z).
        sub(x, z) :- sub(x, y), sub(y, z), sub(z, x). *)
