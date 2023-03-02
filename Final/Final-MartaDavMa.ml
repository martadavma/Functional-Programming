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

(** Distribution of points: 10 + 20 + 10 + 10 + 10 = 60 points 
  Q1 : DONE + TESTED
  Q2 - p1 : DONE + TESTED
     - p2 : DONE + TESTED
  Q3 : DONE + TESTED
  Q4 : DONE + TESTED
  Q5 - a : DONE (ADD PDF)
     - b : DONE
*)

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

  let rec buy (dollars : int) (history : int list) : int =
    match history with
    | [] -> dollars (* just solt the last crytops, or not worth to buy again *)
    | d1 :: tl ->
      match tl with
      | [] -> dollars (* only one day left, can't buy, no time to sell *)
      | d2 :: tl2 -> 
        if d1 > d2 then (buy dollars tl)(* better to buy at d2 *)
        else (sell (dollars/d1) (dollars mod d1) tl) (* buy all *)
    and 

    sell (cryptos : int) (usdLeft : int) (history : int list) : int =
      match history with
      | [] -> initial_investment (* should never happen since this method is only 
                                    called when there is at least one value in hsotiry *)
      | d1 :: tl ->
        match tl with
        |[] -> ((cryptos * d1) + usdLeft) (* no more days left, sell all at d1, day left*)
        | d2 :: tl2 -> 
          if d1 < d2 then (sell cryptos usdLeft tl)(* better to sell at d2 *)
          else (buy ((cryptos * d1) + usdLeft) tl)(* we sell all cryptos *)
  in

  buy initial_investment history
  




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
    | _ -> None
  in
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

type used_v = (string, bool) Pmap.t

(*
   We will create a map that stores the usage of each variable. A used variable can 
   not be removed. However, a variable that is never used (false in used_v) can be 
   removed, and from Let1(v, e1, e2) only e2 would have to be keept, since the rest 
   is never used.
*)
let deadCodeElim (e : expr1) : expr1 =

  (* make a map of the variables that have been used. For every variable that is 
     defined but never used, v * false, else v * true. *)
  let rec evalBool (e : expr1) (envBool : used_v) : used_v =
    match e with
    | Float1 _ -> envBool
    | Plus1(e1, e2) | Minus1(e1, e2) | Mult1(e1, e2) -> 
        (evalBool e2 (evalBool e1 envBool))
    | Let1(v, e1, e2) -> evalBool e2 (Pmap.add v false envBool)
    | Var1(v) -> 
      let opb = Pmap.find v envBool in
      match opb with
      | None -> (Pmap.add v false envBool) 
      | Some b ->
        match b with
        | true  -> envBool
        | false -> (Pmap.add v true envBool)
  in

  (* given a final used_v map, it replaces all Let unused variables with it's e2. *)
  let rec deleteLet (e : expr1) (envBool : used_v) : expr1 = 
    match e with
      | Float1 _ -> e
      | Plus1(e1, e2) -> Plus1(deleteLet e1 envBool, deleteLet e2 envBool)
      | Minus1(e1, e2) -> Minus1(deleteLet e1 envBool, deleteLet e2 envBool)
      | Mult1(e1, e2) -> Mult1(deleteLet e1 envBool, deleteLet e2 envBool)
      | Var1 _ -> e
      | Let1(v, e1, e2) ->
        let mb = Pmap.find v envBool in
        match mb with
        | None -> e (* something is wrong, should never happen *)
        | Some b ->
          match b with
          | true  -> Let1(v, deleteLet e1 envBool, deleteLet e2 envBool)
          | false -> (deleteLet e2 envBool) (* e1 was never used, so no need 
                                               to compute. *)
  in

  deleteLet e (evalBool e Pmap.empty)

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

(* expresion e, bool b - If e used once b = false, else b = true*)
type known_e = (expr1, bool) Pmap.t
(* for assigning variables. If above true, then search for variable, if does not 
  exist, create a let and a variable *)
type new_v = (expr1, string) Pmap.t

(* LIMITATION: can only store up to 26 extra variables, but could be modify to 
   accept more by adding an if condition when varCnt > 122 to create aa, ab,... *)
let commonSubexprElim (e : expr1) : expr1 =
  (* counter to go from an int to a character by Char.chr, and then to string by 
    Char.escaped (Char.chr 98) *)
  let varCnt = ref 96
  in 
  (* stores all new variables *)
  let newVar = ref Pmap.empty
  in
  
  (* make a map of the expression that are repeated. For every expression that 
    is used once e * false, else e * true. *)
  let rec evalBool (e : expr1) (envBool : known_e) : known_e =
    match e with
    | Float1 _ -> envBool
    | Var1(v) -> envBool
    | Let1(v, e1, e2) -> evalBool e2 (evalBool e1 envBool)
    | Plus1(e1, e2) | Minus1(e1, e2) | Mult1(e1, e2) -> 
        let opb = Pmap.find e envBool in
        match opb with
        | None -> evalBool e2 (evalBool e1 (Pmap.add e false envBool))
        | Some b ->
          match b with
          | true  -> evalBool e2 (evalBool e1 envBool)
          | false -> evalBool e2 (evalBool e1 (Pmap.add e true envBool))
  in

  (* given a final known_e map, it replaces all the expressions useed more than
     oncewith a variable *)
  let rec deleteRepE (e : expr1) (envBool : known_e): expr1 = 
    match e with
      | Float1 _ -> e
      | Var1 _ -> e
      | Let1(v, e1, e2) -> Let1(v, (deleteRepE e1 envBool), 
                                (deleteRepE e2 envBool))
      | Plus1(e1, e2) | Minus1(e1, e2) | Mult1(e1, e2) ->
        let mpb = Pmap.find e envBool in
        match mpb with 
        | None -> e (* should never happen *)
        | Some b -> 
          match b with
          | false -> findLet e envBool (* expression used once, no new var *)
          | true  -> (* expression used more than once *)
            let mpv = Pmap.find e !newVar in
            match mpv with
            | None -> (* create variable and emit let *)
                varCnt := !varCnt + 1; 
                newVar := Pmap.add e (Char.escaped (Char.chr !varCnt)) !newVar;
                Let1((Char.escaped (Char.chr !varCnt)), e, (Float1 0.)) 
                                        (* Float1 0. is removed later *)
            | Some var -> Var1 var (* substitute e with var *)
                          
  (* find Let before Plus, Minus, Mult*)
  and findLet (e : expr1) (envBool : known_e) : expr1 = 
    match e with
    | Float1 _ | Var1 _ | Let1 _ -> e (* should never happen *)
    | Plus1(e1, e2) | Minus1(e1, e2) | Mult1(e1, e2) -> 
      let letE = deleteRepE e1 envBool in
        match letE with
        | Let1(v, e1l, _) -> Let1(v, e1l, findE e v envBool)
        | Float1 _ | Var1 _ -> plMiMu e envBool 
        | Plus1(e1, e2) | Minus1(e1, e2) | Mult1(e1, e2) -> plMiMu e envBool 

  (* find expression to send to deleteRepE *)
  and findE (e : expr1) (var : string) (envBool : known_e) : expr1 =
    match e with
    | Float1 _ | Var1 _ | Let1 _ -> e (* should never happen *)
    | Plus1(e1, e2) -> Plus1(Var1(var), (deleteRepE e2 envBool))
    | Minus1(e1, e2) -> Minus1(Var1(var), (deleteRepE e2 envBool))
    | Mult1(e1, e2) -> Mult1(Var1(var), (deleteRepE e2 envBool))

  (* for expressions that do not have to be added nor are in var. *)
  and plMiMu (e : expr1) (envBool : known_e) : expr1 = 
    match e with
    | Float1 _ | Var1 _ | Let1 _ -> e (* should never happen *)
    | Plus1(e1, e2) -> Plus1((deleteRepE e1 envBool),
                              (deleteRepE e2 envBool))
    | Minus1(e1, e2) -> Minus1((deleteRepE e1 envBool), 
                                (deleteRepE e2 envBool))
    | Mult1(e1, e2) -> Mult1((deleteRepE e1 envBool), 
                              (deleteRepE e2 envBool))
    in

    deleteRepE e (evalBool e Pmap.empty)

(* Explain your solution. 
      Our solution first makes a Pmap (e * b) which stores expr1 e as true if it 
      has been used more than once, and false if it has been used only once.
      Based on this Pmap which we call known_e and is calculated in "evalBool",
      we then iterate over the expression in "deleteRepE". When a float or a variable
      are found, these are returned directly, since they obvioulsy can not be sustituded 
      by a new variable. When a let is found, both parts of the expression are verified
      to see if any subexpressions could be sustituded. When a Plus, Minus or Mult are
      founs is when the complications arise. We first check if the expression is true or
      false inside the Pmap. If it is false it means that the expression itself is never
      repeat. However, this could mean that the expression is the beginning of a Let
      expression. For isntance Plus((x + y), (x +y)...) would be false, however it initates
      a Let, as Let (z, x+y, plus (z, (x+y)...)). So this is what we check in "findLet"
      and "findE". If it is not the case where the above happens, and it is just Plus,
      Minus or Mult, then we simply call recursively "deleteRepE" on e1 and e2.

   Are there situations where common subexpression elimination would not yield a benefit
   in running time?
      Yes, there are many cases where subexpression elimination does not reduce the
      evaluation cost.
      For instance:
      let e1 = Mult1 (Plus1 (Var1 "x", Var1 "y"), Minus1 (Float1 4., Plus1 (Var1 "x", Var1 "y"))).
      That is: e1 = (x + y) * (4 - (x + y))
      after running commonSubexprElim e1, we get
      e1' = Let1 ("a", Plus1 (Var1 "x", Var1 "y"), Mult1 (Var1 "a", Minus1 (Float1 4., Var1 "a"))).
      That is: a = (x + y) in a * (4 - a)
      Since the substituded subexpression was only used twice in e1, and it only had a cost of 
      one, the cost of the total uses of (x+y) in e1 was 1 * 2.
      However, creating the variable already has a cost of two, since there is a Let1 and the
      Plus1 itself. As a result, the cost of creating variable a in e1' is 2, which is the same.
      As a result, both e1 and e1' evaluate to 9 when runing "evalCost e1" and "evalCost e1'".
    *)





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

(* given a variable expr1, returns the whole expr1 it represents.*)
type var_m = (string, expr1) Pmap.t

(* we will relay only in the rule of product.
   In addition, when any Let is found, we add the variable to the Pmap above and  *)
let partialDerivative (x : string) (e : expr1) : expr1 =
  (* when applying the product rule, if no more derivation needed, we have to make sure
     that no let variables are used, but only the final expressions. *)
  let rec findVar (exp : expr1) (vars : var_m): expr1 =
    match exp with
    | Float1 _ -> print_endline "- findVar - FLOAT"; exp
    | Plus1(e1, e2) -> print_endline "- findVar - PLUS"; 
                       Plus1(findVar e1 vars, findVar e2 vars)
    | Minus1(e1, e2) -> print_endline "- findVar - MINUS"; Minus1(findVar e1 vars, findVar e2 vars)
    | Mult1(e1, e2) -> print_endline "- findVar - MULT"; Mult1(findVar e1 vars, findVar e2 vars)
    | Let1(v, e1, e2) -> print_endline "LET - var added"; findVar e2 (Pmap.add v e1 vars)
    | Var1(v) -> print_endline "- findVar - VAR - ";
      let vmp = Pmap.find v vars in
      match vmp with
      | None -> print_endline "VAR - NO vmp found"; exp
      | Some expr -> print_endline "VAR - vmp found"; findVar expr vars
  in

  let rec derivate (exp : expr1) (vars : var_m): expr1 =
    print_endline "----------------------";
    match exp with
    | Float1 _ -> print_endline "FLOAT"; Float1(0.)
    | Let1(v, e1, e2) -> print_endline "LET - var added"; derivate e2 (Pmap.add v e1 vars)
    | Plus1(e1, e2) -> print_endline "PLUS"; Plus1(derivate e1 vars, derivate e2 vars)
    | Minus1(e1, e2) -> print_endline "MINUS"; Minus1(derivate e1 vars, derivate e2 vars)
    | Mult1(e1, e2) -> print_endline "MULT"; Plus1(Mult1((findVar e1 vars), derivate e2 vars), Mult1(derivate e1 vars, (findVar e2 vars)))
    | Var1(v) -> print_endline "VAR - ";
      let vmp = Pmap.find v vars in
      match vmp with
      | None -> print_endline "VAR - NO vmp found"; if v = x then Float1 1.
                else Float1 0.
      | Some expr -> print_endline "VAR - vmp found"; derivate expr vars
  in

  derivate e (Pmap.empty)





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
  
  let matchDim (d1 : dimensions option) (d2 : dimensions option) : dimensions option =
    match d1,d2 with
    | None,_ -> None
    | _,None -> None
    | Some di1, Some di2 ->if di1 = di2 then d1
                     else None
  in

  let multDim (d1 : dimensions option) (d2 : dimensions option) : dimensions option =
    match d1,d2 with
    | None,_ -> None
    | _,None -> None
    | Some di1, Some di2 ->
        match di1, di2 with
        | (l1, m1, t1), (l2, m2, t2) -> Some ((l1 + l2), (m1 + m2), (t1 + t2))
  in

  let divDim (d1 : dimensions option) (d2 : dimensions option) : dimensions option =
    match d1,d2 with
    | None,_ -> None
    | _,None -> None
    | Some di1, Some di2 ->
        match di1, di2 with
        | (l1, m1, t1), (l2, m2, t2) -> Some ((l1 - l2), (m1 - m2), (t1 - t2))
  in

  match e with
  | Float2 (f, d) -> Some d
  | Plus2(e1, e2) -> matchDim (dim2 e1) (dim2 e2)
  | Minus2(e1, e2) ->matchDim (dim2 e1) (dim2 e2)
  | Mult2(e1, e2) ->multDim (dim2 e1) (dim2 e2)
  | Div2(e1, e2) ->divDim (dim2 e1) (dim2 e2)

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

      * See attached PDF for the procedence graphs. *

      - path(x, y) :- edge(x, y).
        path(x, z) :- edge(x, y), path(y, z).
        nopath(x, y) :- !path(x, y).

        These queries are well-stratified. When building the procedence graph
        we can se that ther is a positive edge from edge() to path(), and from
        path() to itself. Then we see a negative edge from path() to nopath().
        However, there are no cycles with a negative edge, and therefore this 
        Datalog program is stratifiable.

      - even("zero").
        even(z) :- even(x), succ(x, y), succ(y, z).
        odd(x) :- !even(x).

        These queries are well-stratified, they have a solution since are no 
        cycles with a negative edge.

      - even("zero").
        even(y) :- odd(x), succ(x, y).
        odd(y) :- even(x), succ(x, y).

        These queries are well-stratified, there is no negation and a program can be built.

      - even("zero").
        odd(x) :- !even(x).
        even(y) :- odd(x), succ(x, y).

        These queries are no well-stratified, as we can see in the attached PDF, there is a 
        cycle between even and odd, and from even to odd the edge is negative. This results
        in a program not possible to create.

   b. The following programs contain an unnecessarily large number of rules. How
      many of them can you delete without changing the meaning of the overall
      program? Explain your responses. As before, assume reasonable schemas for
      all relations.

      - path(x, y) :- edge(x, y).
        path(x, z) :- edge(x, y), path(y, z).
        path(x, z) :- path(x, y), edge(y, z).

        Rules 2 and 3 can be removed and substituded by:  path(x, z) :- path(x, y), path(y, z).

      - step(x, y) :- edge(x, y).
        step(x, y) :- step(x, y), step(x, u).

        Rule 2 adds no new information, since there already was a step(x, y) in the if conndition
        the else condition does not modify or add anything. Therefore it can be removed without
        changing the meaning of the overall program.

      - sub(x, y) :- R(x, y).
        sub(x, z) :- sub(x, y), sub(y, z).
        sub(x, z) :- sub(x, y), sub(y, z), sub(z, x). 
        
        Again, rule 3 adds no information. Rule 2 already stablishes that if sub(x, y), sub(y, z)
        then sub(x, z). Therefore, in Rule 3 we are just saying the same with more constrains,
        so if the if part of rule 3 is true, the if part of rule 2 is true, and both result on the
        same else, so there is no need for rule 3. Therefore rule 3 can be reomved without changing
        the meaning of the overall program.

        *)


