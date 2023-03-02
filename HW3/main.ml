(*******************************************************************************
Homework 3
==========
CSCI 499, Fall 2022: An Introduction to Functional Programming
Mukund Raghothaman
Due: 10pm PT on 11 November, 2022
*******************************************************************************)

(*******************************************************************************
- Your name: Marta Davila Mateu
- Your partner's name (if any): Logan Norman
*******************************************************************************)

(*******************************************************************************
Instructions
------------
1. Please setup your programming environment by following the steps outlined in
   the cheatsheet and on the course website.

2. Please feel free to collaborate with a partner, but make sure to write your
   submission independently.

3. This assignment consists of five files: main.ml (this file), imp.ml
   (describing the abstract syntax of the Imp language), cipher.ods (which you
   can open either using Microsoft Excel or Libreoffice Calc), and two build
   scripts: build.sh and clean.sh.

4. Please execute build.sh to be able to load this file into the toplevel:

     $ ./build.sh

5. IMPORTANT: Make sure that I can cleanly import this file into the Ocaml
   toplevel when I call #use "main.ml". Comment out any questions you were
   unable to solve / have left unanswered.

6. As part of your submission, you will create two additional files,
   implexer.mll and impparser.mly. Place all seven files in a directory with
   your name, zip the directory, and submit the resulting archive using
   Blackboard.
*******************************************************************************)

#load "imp.cmo";;

(* Uncomment the following lines once you have finished creating implexer.mll
   and impparser.mly and build.sh does not produce any errors: *)

#load "impparser.cmo";;
#load "implexer.cmo";;

open Imp

exception NotImplemented

(** Distribution of points: 10 + 20 + 15 + 15 + 15 + 15 = 90 points *)

(*
E1 : DONE
E2 : DONE
E3 : DONE
E4 : DONE
E5 : DONE
E6 : DONE
*)




(******************************************************************************)
(* Question 1: Interpreting an Imperative Language, Part 1 (3 + 3 + 4 points)

   In this question and the next, we will build an end-to-end interpreter for a
   toy imperative language, sometimes called Imp. Here is an example program in
   Imp:

     x = 5;
     while (0 < x) do
       output x;
       x = x - 1
     done

   Executing this program results in the list of integers [5; 4; 3; 2; 1].

   In this question, we will write the core of the interpreter, which starts
   from the AST of the program and does the actual evaluation. In the next
   question, we will write the parser which takes a string representation of the
   program and produces the AST. Chaining the two together will give us the
   end-to-end interpreter.

   Constructs in Imp belong to one of three syntactic categories, arithmetic
   expressions, Boolean expressions and commands, and we will represent these
   using the types Imp.aexp, Imp.bexp and Imp.cmd respectively. See their
   definitions in the supplementary file imp.ml.

   Recall your solution to Question 7b from Homework 1. To evaluate an
   arithmetic expression, such as "x + 5", it is necessary to know the value of
   the variable "x". We will record the current value of each variable in a map
   which associates their names with their values. This map, which we will call
   the "evaluation environment", will be of type: *)

type env_t = (string, int) Pmap.t

(* The empty environment is given by Pmap.empty, and variables may be set and
   queried using the Pmap.add and Pmap.find functions respectively. To simplify
   matters, all variables which are not explicitly defined in the environment
   have the initial value 0.

   Fill in the functions aeval, beval, and ceval. We have included some
   assertions in comments which you may use to test your solutions. *)

let findVar (env : env_t) (s : string) : int =
  match (Pmap.find s env) with
  | Some v -> v
  | None -> 0 

let rec aeval (env : env_t) (a : aexp) : int =
  match a with
  | Int(i) -> i
  | Var(s) -> findVar env s
  | Plus(a1, a2) -> (aeval env a1) + (aeval env a2)
  | Minus(a1, a2) -> (aeval env a1) - (aeval env a2)

(* let _ =
  let env0 = Pmap.empty in
  let env1 = Pmap.add "x" 5 env0 in

  let a1 = Int 3 in
  let a2 = Var "x" in
  let a3 = Plus(a1, a1) in
  let a4 = Minus(a2, a1) in

  let _ = assert ((aeval env0 a1) = 3) in
  let _ = assert ((aeval env0 a2) = 0) in
  let _ = assert ((aeval env1 a2) = 5) in
  let _ = assert ((aeval env0 a3) = 6) in
  let _ = assert ((aeval env1 a4) = 2) in
  () *)
  
let rec beval (env : env_t) (b : bexp) : bool =
  match b with
  | Bool(bo) -> bo
  | Lt(a1, a2) -> (aeval env a1) < (aeval env a2) 
  | Leq(a1, a2) -> (aeval env a1) <= (aeval env a2)
  | Eq(a1, a2) -> (aeval env a1) = (aeval env a2)
  | And(b1, b2) -> ((beval env b1) && (beval env b2))
  | Or(b1, b2) -> ((beval env b1) || (beval env b2))
  | Not(b1) ->
        match (beval env b1) with
        | true -> false
        | false -> true 

(* let _ =
  let env0 = Pmap.empty in
  let env1 = Pmap.add "x" 3 env0 in
  let env2 = Pmap.add "y" 5 env1 in

  let ax = Var "x" in
  let ay = Var "y" in

  let b1 = Bool false in
  let b2 = Lt(ax, ay) in
  let b3 = Leq(ax, Plus(ax, ay)) in
  let b4 = Eq(Plus(ax, ay), Plus(ay, ax)) in
  let b5 = And(b2, b3) in
  let b6 = Or(b1, b3) in
  let b7 = Not(b1) in

  let _ = assert (not (beval env2 b1)) in
  let _ = assert (beval env2 b2) in
  let _ = assert (beval env2 b3) in
  let _ = assert (beval env2 b4) in
  let _ = assert (beval env2 b5) in
  let _ = assert (beval env2 b6) in
  let _ = assert (beval env2 b7) in
  () *)

let ceval (c : cmd) : int list =

  let env = ref Pmap.empty in

  let rec helpEval (cm : cmd) (l : int list) : int list =
    match cm with 
    | Output(a) -> l @ [aeval !env a]
    | Asgn(s,a2) -> 
      env := Pmap.add s (aeval !env a2) !env; l
    | Skip -> l
    | Seq(c1, c2) -> helpEval c2 (helpEval c1 l)
    | IfElse(b, c1, c2) ->
      if (beval !env b) then helpEval c1 l
      else helpEval c2 l
    | While(b, c1) ->
      if (beval !env b) then helpEval (While(b, c1)) (helpEval c1 l)
      else l
  in

  helpEval c []

(* let _ =
  let ax = Var "x" in
  let ay = Var "y" in
  let a0 = Int 0 in
  let a1 = Int 1 in
  let a5 = Int 5 in
  let c = Seq(Asgn("x", a5),
              While(Lt(a0, ax),
                    Seq(Output(ax),
                    Seq(Asgn("x", Minus(ax, a1)),
                    Seq(Output(ay), Asgn("y", Plus(ay, a1))))))) in
  assert (ceval c = [5; 0; 4; 1; 3; 2; 2; 3; 1; 4]) *)






(******************************************************************************)
(* Question 2: Interpreting an Imperative Language, Part 2 (10 + 10 points)

   We will now write the front-end of the interpreter. Construct a lexical
   analyzer using Ocamllex in a file named "implexer.mll", and construct a
   parser using Menhir in a file named "impparser.mly".

   Here is the grammar describing the syntax of Imp:

     Commands,
     cmd ::= output aexp
           | var-name "=" aexp
           | "skip"
           | cmd ";" cmd (* Execute the first command, followed by the second
                            command *)
           | "if" bexp "then" cmd "else" cmd "fi"
           | "while" bexp "do" cmd "done"
           | "(" cmd ")"

     Arithmetic expressions,
     aexp ::= integer-literal (* Any non-empty sequence of digits from '0' to
                                 '9', possibly prefixed with a '-' *)
            | variable-name   (* Variable names begin with an alphabet or an
                                 underscore character, and are followed by zero
                                 or more alphabets, digits, or underscore
                                 characters. *)
            | aexp "+" aexp   (* Both "+" and "-" are left-associative, so that
                                 "a - b + c" is parsed as "(a - b) + c". *)
            | aexp "-" aexp
            | "(" aexp ")"

     Boolean expressions,
     bexp ::= "true"
            | "false"
            | aexp "<" aexp
            | aexp "<=" aexp
            | aexp "==" aexp
            | bexp "&&" bexp
            | bexp "||" bexp
            | "!" bexp
            | "(" bexp ")"

   For full credit:
   1. Make sure that to resolve precedence correctly with the Boolean operators.
      The negation operator, "!" binds the tightest of them all, and the
      disjunction operator "||" is the loosest. In particular, "true || false &&
      true" should be parsed as "true || (false && true)", "true && false ||
      true" should be parsed as "(true && false) || true", and "!false && true"
      should be parsed as "(!false) && true" respectively.
   2. Ensure that both "implexer.mll" nor "impparser.mly" produce no warnings
      when compiled using the respective tools.

   Define the following function parse : string -> int list which takes the
   textual representation of an Imp program as input, and produces its list of
   integers as output. *)

  let parse (str : string) : cmd =
    Impparser.prog Implexer.read (Lexing.from_string str)
  
  let eval (str : string) : int list = str |> parse |> ceval

(* At this point, I will test your implementation with: *)

(* let _ =
  let cs = "x = 5;
            while (0 < x) do
              output x;
              x = x - 1;
              output y;
              y = y + 1
            done" in
  assert (eval cs = [5; 0; 4; 1; 3; 2; 2; 3; 1; 4]) *)





(******************************************************************************)
(* Question 3: Matching Regular Expressions in Linear Time (5 + 5 + 5 points)

   In class, we outlined a single pass linear time regular expression matching
   algorithm. While it is an educational experience to implement this algorithm
   in its full generality, it would be overwhelming for a homework question. We
   will instead implement fast matching algorithms for some selected regular
   expressions.

   For full credit, your program must be tail recursive, run in linear time, and
   make a single left-to-right pass over the input string.

   You may find the following function useful: *)

let string_to_list (str: string) : char list =
  str |> String.to_seq |> List.of_seq


(* - Write a program that determines whether a given string matches the pattern
     "Hi"*: *)
let matchesHiStar (str : string) : bool =
  let rec matchMaker (l : char list ) : bool =
    match l with
    | [] -> true
    | hd1 :: tl1 ->
      if hd1 == 'H' then
        match tl1 with 
        | [] -> false
        | hd2 :: tl2 ->
          if hd2 == 'i' then matchMaker tl2
          else false
      else false
  
  in
  matchMaker (string_to_list str)


(* - Write a program that determines whether a given string matches the pattern
     ("Hi" | "Hello")*: *)
let matchesHiOrHelloStar (str : string) : bool =

  (* checks if the following character is an i *)
  let iMatch (l : char list ) : char list option =
    match l with
    | [] -> None
    | hd :: tl ->
      if hd == 'i' then Some tl
      else None
  in

  (* checks if the following characters are "ello" *)
  let elloMatch (l : char list ) : char list option =
    match l with
    | [] -> None
    | e :: l :: l2 :: o :: tl ->
      if (e == 'e') && (l == 'l') && (l2 == 'l') && (o == 'o') then Some tl
      else None
    | hd :: tl (* less than 4 elements*) -> None 
  in

  (* acts as main functionallty *)
  let rec matchMaker (l : char list ) : bool =
    match l with
    | [] -> true
    | hd :: tl ->
      if hd == 'H' then
        match (iMatch tl) with
        | Some li -> matchMaker li
        | None -> 
          match (elloMatch tl) with
          | Some lello -> matchMaker lello
          | None -> false
      else false
    in
    let chList = string_to_list str in
    matchMaker chList


(* - Write a program that determines whether a given string matches the pattern
     ("Hi" | "Hello")* . "Okay"*: *)
let matchesHiOrHelloStarThenOkayStar (str : string) : bool =
  (* checks if the following character is an i *)
  let iMatch (l : char list ) : char list option =
    match l with
    | [] -> None
    | hd :: tl ->
      if hd == 'i' then Some tl
      else None
  in

  (* checks if the following characters are "ello" *)
  let elloMatch (l : char list ) : char list option =
    match l with
    | [] -> None
    | e :: l :: l2 :: o :: tl ->
      if (e == 'e') && (l == 'l') && (l2 == 'l') && (o == 'o') then Some tl
      else None
    | hd :: tl (* less than 4 characters *) -> None 
  in

  (* checks if the following characters are "kay" *)
  let kayMatch (l : char list ) : char list option =
    match l with
    | [] -> None
    | k :: a :: y :: tl ->
      if (k == 'k') && (a == 'a') && (y == 'y') then Some tl
      else None
    | hd :: tl (* less than 3 characters *) -> None 
  in

  (* acts as main functionallity and checks seconds part of the pattern: "Okay"* *)
  let rec secondMatch (l : char list ) : bool =
    match l with
    | [] -> true
    | hd :: tl ->
      if hd == 'O' then
        match (kayMatch tl) with
        | Some lkay -> secondMatch lkay
        | None -> false
      else false
  in

  (* checks fitst part of the pattern: ("Hi" | "Hello")* *)
  let rec firstMatch (l : char list ) : bool =
    match l with
    | [] -> true
    | hd :: tl ->
      if hd == 'H' then
        match (iMatch tl) with
        | Some li -> firstMatch li
        | None -> 
          match (elloMatch tl) with
          | Some lello -> firstMatch lello
          | None -> false
      else secondMatch l
  in

  let chList = string_to_list str in
  firstMatch chList





(******************************************************************************)
(* Question 4: A Simple Type Checker (15 points)

   We define here the abstract syntax of a simple expression language. Observe
   that we have eliminated the distinction between arithmetic and Boolean
   expressions, and variables may hold values of either type: *)

type expr =
  | Var of string                    (* Variables *)
  | IntLit of int                    (* Integer literals *)
  | Plus of expr * expr              (* Integer addition *)
  | Minus of expr * expr             (* Integer subtraction *)
  | BoolLit of bool                  (* Boolean literals *)
  | And of expr * expr               (* Boolean conjunctions *)
  | Or of expr * expr                (* Boolean disjunctions *)
  | Not of expr                      (* Boolean negations *)
  | Leq of expr * expr               (* Arithmetic inequality *)
  | IfThenElse of expr * expr * expr (* Conditional expressions *)

(* As usual, the eval function evaluates the expression in an environment that
   maps variable names to their corresponding values. The main difference is
   that expressions may evaluate to either integers or to Boolean values, or may
   be undefined, depending on the environment in which they are evaluated. We
   create a uniform way to represent the two types of values, and are careful
   while defining the evaluation function: *)

type value =
  | IntVal of int
  | BoolVal of bool

let rec eval (env : (string, value) Pmap.t) (e : expr) : value option =
  let opt_apply f o1 o2 =
    match (o1, o2) with
    | (Some c1, Some c2) -> f c1 c2
    | _ -> None in

  let zz_opt_apply f o1 o2 =
    let f' c1 c2 =
      match (c1, c2) with
      | (IntVal v1, IntVal v2) -> Some (f v1 v2)
      | _ -> None in
    opt_apply f' o1 o2 in

  let bb_opt_apply f o1 o2 =
    let f' c1 c2 =
      match (c1, c2) with
      | (BoolVal v1, BoolVal v2) -> Some (f v1 v2)
      | _ -> None in
    opt_apply f' o1 o2 in

  let z_inject ov = Option.map (fun v -> IntVal v) ov in
  let b_inject ov = Option.map (fun v -> BoolVal v) ov in

  match e with
  | Var v -> Pmap.find v env
  | IntLit c -> Some (IntVal c)
  | Plus(e1, e2) -> z_inject (zz_opt_apply (+) (eval env e1) (eval env e2))
  | Minus(e1, e2) -> z_inject (zz_opt_apply (-) (eval env e1) (eval env e2))
  | BoolLit c -> Some (BoolVal c)
  | And(e1, e2) -> b_inject (bb_opt_apply (&&) (eval env e1) (eval env e2))
  | Or(e1, e2) -> b_inject (bb_opt_apply (||) (eval env e1) (eval env e2))
  | Not e1 ->
      (match eval env e1 with
       | Some (BoolVal v1) -> Some (BoolVal (not v1))
       | _ -> None)
  | Leq(e1, e2) -> b_inject (zz_opt_apply (<=) (eval env e1) (eval env e2))
  | IfThenElse(e1, e2, e3) ->
      (match eval env e1 with
       | Some (BoolVal v1) -> if v1 then eval env e2 else eval env e3
       | _ -> None)

(* Let's perform some sanity checks: *)

let _ =
  let env0 = Pmap.empty in
  let env1 = Pmap.add "x" (IntVal 5) env0 in
  let env2 = Pmap.add "y" (BoolVal true) env1 in

  let e1 = IntLit 3 in
  let e2 = Var "x" in
  let e3 = Var "y" in
  let e4 = Plus(e2, e2) in
  let e5 = Plus(e2, e3) in
  let e6 = And(e3, e3) in
  let e7 = And(e3, e2) in

  let _ = assert (eval env0 e1 = Some (IntVal 3)) in
  let _ = assert (eval env0 e2 = None) in
  let _ = assert (eval env2 e2 = Some (IntVal 5)) in
  let _ = assert (eval env2 e3 = Some (BoolVal true)) in
  let _ = assert (eval env2 e4 = Some (IntVal 10)) in
  let _ = assert (eval env2 e5 = None) in
  let _ = assert (eval env2 e6 = Some (BoolVal true)) in
  let _ = assert (eval env2 e7 = None) in
  ()

(* Notice that whether an expression results in an output depends on the environment
   being provided. For example, the expression e2 can be successfully evaluated in the
   environment e1, but fails to produce an output in environment e2. *)

let _ =
  let env0 = Pmap.empty in
  let env1 = Pmap.add "x" (IntVal 5) env0 in
  let env2 = Pmap.add "x" (BoolVal true) env1 in

  let e1 = Var "x" in
  let e2 = Plus(e1, e1) in

  let _ = assert (eval env1 e2 = Some (IntVal 10)) in
  let _ = assert (eval env2 e2 = None) in
  ()

(* Two determine whether an expression can be successfully evaluated in an
   environment, we create a simple type system for the language, consisting of
   two types: *)

type expr_type = IntType | BoolType

(* An environment type is a mapping from variable names to their corresponding
   types. One can regard this as a guarantee from the user that each variable is
   of the type being asserted. Alternatively, think of this as the symbol table
   maintained by a compiler. *)

type env_type = (string, expr_type) Pmap.t

(* Write a function get_type which takes an environment type and an expression,
   and determines whether the expression can be successfully evaluated or not.
   In particular, if your function evaluates to (Some IntType), then the
   expression should always evaluate to a result of the form (IntVal _) in
   conforming environments, and if it returns (Some BoolType), then evaluating
   the expression should always result in a value of type (BoolVal _). *)

let get_type (gamma : env_type) (e : expr) : expr_type option =
  (* transform a gamma enviroment to an enviroment accepted by eval. *)
  let rec gammaToEnv (l : (string * expr_type) list) (env : (string, value) Pmap.t) : 
  ((string, value) Pmap.t) =
    match l with
    | [] -> env
    | hd :: tl ->
      match snd hd with
      | IntType -> gammaToEnv tl (Pmap.add (fst hd) (IntVal 1) env)
      | BoolType -> gammaToEnv tl (Pmap.add (fst hd) (BoolVal true) env)
  in

  (* since I can't do a match inside a match, this method matches the empty list *)
  let matchEmptyEnv (e : expr) : expr_type option =
    match (eval Pmap.empty e) with
      | Some IntVal(i) -> Some IntType
      | Some BoolVal(b) -> Some BoolType
      | None -> None
  in

  match (Pmap.to_alist gamma) with
  | [] -> matchEmptyEnv e
  | hd :: tl ->
    match (eval (gammaToEnv (Pmap.to_alist gamma) Pmap.empty) e) with
    | Some IntVal(i) -> Some IntType
    | Some BoolVal(b) -> Some BoolType
    | None -> None

(* If your solution is correct, then the following assertions will hold for all
   expressions e and environments env: *)

let get_type_test (env : (string, value) Pmap.t) (e : expr) =
  let val_type (v : value) : expr_type =
    match v with
    | IntVal _ -> IntType
    | BoolVal _ -> BoolType in

  let gamma = Pmap.map val_type env in

  match (get_type gamma e, eval env e) with
  | (Some IntType, Some (IntVal _)) -> assert true
  | (Some IntType, _) -> assert false
  | (Some BoolType, Some (BoolVal _)) -> assert true
  | (Some BoolType, _) -> assert false
  | _ -> assert true

(* Disclaimer: The above tests are not exhaustive! Some of you may notice that
   defining:

   let get_type env_type e = None

   will pass the test cases, and also passes the requirements in a technical
   sense. However they do not perform any interesting analysis on the
   expressions in question. *)





(******************************************************************************)
(* Question 5: Incremental Evaluation in Spreadsheets (8 + 7 = 15 points)

   One important feature of spreadsheet applications is their ability to
   automatically recompute cells when the user changes the data. Engineers have
   to be careful while implementing this feature, since it can make the program
   unresponsive when the spreadsheet is large. Efficiently recomputing the
   results when the input data changes is an important problem in many practical
   applications. One common optimization is to use the dependence relation
   between the cells to only recompute those cells which might have changed.

   In this question, we will study this optimization.

   We start by defining "cells". A cell has an address, which we represent as a
   pair of integers: the indices of the row and column in which it appears. This
   is somewhat different from the traditional alphanumeric addressing (e.g.
   "G5"), but converting from one representation to the other is trivial. *)

type cell_address = int * int

(* For the purposes of this question, we are unconcerned with the formulas
   inside the cells, we only worry about their mutual dependencies. We will
   represent these dependencies as a list of pairs,

     deps : (cell_addrress * cell_address) list

   Each element (c1, c2) of deps indicates that the value of c2 is dependent on
   the value of c1. Recall that for the spreadsheet to be well-formed, these
   dependencies must form a DAG. Write a function to determine whether this is
   the case. *)

(* we will use Kahn's algorithm to find a topological order, if no such order 
   exist, then the there is a cycle. *)
let is_dag (deps : (cell_address * cell_address) list) : bool =

  (* add node to list if it does not exist *)
  let rec addDegreeLeft (checked : (cell_address * int) list) (left : 
  (cell_address * int) list) (address : cell_address) : (cell_address * int) list =
    match left with
    | [] -> checked @ [(address, 0)]
    | hd :: tl ->
      if (fst (fst hd) = fst address) && (snd (fst hd) = snd address) then (checked @ left)
      else addDegreeLeft (checked @ [hd]) tl address
  in

  (* add one to degree of the cell dependent, or add cell to list *)
  let rec addDegreeRight (checked : (cell_address * int) list) (left : 
  (cell_address * int) list) (address : cell_address) : (cell_address * int) list =
    match left with
    | [] -> checked @ [(address, 1)]
    | hd :: tl ->
      if (fst (fst hd) = fst address) && (snd (fst hd) = snd address) then 
        (checked @ [(address, ((snd hd) + 1))] @ tl)
      else addDegreeRight (checked @ [hd]) tl address
  in

  (* creates a list with the degree of each address, where the degree is the number of 
     dependencies *)
  let rec degreeAdress (nodes : (cell_address * cell_address) list) 
  (current : (cell_address * int) list) : (cell_address * int) list =
    match nodes with
    | [] -> current 
    | hd :: tl ->
      degreeAdress tl (addDegreeLeft [] (addDegreeRight [] current (snd hd)) (fst hd))
  in

  (* given a degree Adresses list, returns the list of addresses with degree 0 *)
  let rec degree0 (left : (cell_address * int) list) (current : cell_address list) :
  (cell_address list) =
    match left with
    | [] -> current
    | hd :: tl -> if snd hd = 0 then degree0 tl (current @ [fst hd])
                  else degree0 tl current
  in

  (* given a list and an address, removes the cell and all it's dependencies *)
  let rec compNewList (cell : cell_address) (left : (cell_address * cell_address) list) 
  (current : (cell_address * cell_address) list) : (cell_address * cell_address) list =
    match left with
    | [] -> current
    | hd :: tl -> 
      if (fst (fst hd) = fst cell) && (snd (fst hd) = snd cell) then 
        compNewList cell tl current
      else compNewList cell tl (current @ [hd])
  in

  (* execute Kahn's algorithm in order. We do not need to store the actual 
     topological order, since we are simply returning true or false *)
  let rec compTopSort (left : (cell_address * cell_address) list) : bool =
    match left with 
    | [] -> true
    | hd :: tl ->
      match (degree0 (degreeAdress left []) []) with
      | [] -> false
      | hd0 :: tl0 -> compTopSort (compNewList hd0 left [])
  
  in compTopSort deps

(* If your implementation is correct, then it will pass the following tests:

let _ =
  let c1 = (1, 1) in
  let c2 = (1, 2) in
  let c3 = (2, 1) in
  let c4 = (2, 2) in

  let _ = assert (is_dag [(c1, c2); (c2, c3); (c3, c4) ]) in
  let _ = assert (not (is_dag [(c1, c2); (c2, c3); (c2, c1)])) in
  let _ = assert (not (is_dag [(c1, c1)])) in
  () *)

(* Now write a function that accepts the list of dependencies that describes a
   spreadsheet, and the address of the cell initially changed by the user, and
   determine the cells which need recomputation, and the order in which they
   need to be recomputed. For this function, you may assume that (is_dag deps)
   evaluates to true. *)

let plan_reeval (deps : (cell_address * cell_address) list)
                (init : cell_address) : cell_address list =

  (* cells might have more than one dependecy, we only want the last appearence
     for each unique cell *)
  let rec checkIfAdded (checked : cell_address list) (left : cell_address list)
  (address : cell_address) : cell_address list =
    match left with
    | [] -> checked
    | hd :: tl -> 
      if ((fst hd = fst address) && (snd hd = snd address)) then (* delete from list *)
        checkIfAdded checked tl address
      else (* leave in list *) checkIfAdded (checked @ [hd]) tl address
  in
  
  (* return list of all cells that depend on address and therefore have to be updated *)
  let rec findDependents (left : (cell_address * cell_address) list) 
  (current : cell_address list) (address : cell_address): cell_address list =
     match left with
      | [] -> current
      | hd :: tl -> 
        if (fst (fst hd) = fst address) && (snd (fst hd) = snd address) then 
          findDependents tl (current @ [snd hd]) address
        else findDependents tl current address
  in

  (* main functionallity *)
  let rec addList (dependents : cell_address list) (current : cell_address list) : 
  cell_address list =
    match dependents with
    | [] -> current
    | hd :: tl ->
      addList (tl @ (findDependents deps [] hd)) ((checkIfAdded [] current hd) @ [hd])
  in

  addList (findDependents deps [] init) ([])

(* For example, if:
   1. c1 = (1, 1), c2 = (1, 2), c3 = (2, 1), and c4 = (2, 2),
   2. deps = [(c2, c4); (c1, c2); (c3, c4); (c1, c3)], and
   3. init = c1,
   then (plan_reeval deps init) can evaluate either to [c2; c3; c4], or to
   [c3; c2; c4].

   On the other hand, it should never evaluate to [c2; c4; c3], since the cell
   c4 depends on the cell c3. *)





(******************************************************************************)
(* Question 6: Implementing Substitution Ciphers in a Spreadsheet (15 points)

   In this question, we will construct a simple substitution cipher using a
   spreadsheet. The cipher we implement is called the Vigenere cipher. We begin
   with a plaintext message, such as "CLASSFROMTHREETHIRTYTOFIVETHIRTY", and a
   key, such as "CSCI". It repeats the key as many times as needed to cover the
   plaintext:

     CLASSFROMTHREETHIRTYTOFIVETHIRTY
     CSCICSCICSCICSCICSCICSCICSCICSCI

   It then shifts each letter in the plaintext message by the corresponding
   number of characters to obtain the ciphertext. For example, shifting the
   plaintext character C by the key character C results in the character C + C =
   C + 2 = E. Similarly, shifting the plaintext character L by the key character
   S results in the ciphertext character L + S = L + 18 = D. Taken together,
   this would result in the ciphertext:

     CLASSFROMTHREETHIRTYTOFIVETHIRTY
     CSCICSCICSCICSCICSCICSCICSCICSCI
     --------------------------------
     EDCAUXTWOLJZGWVPKJVGVGHQXWVPKJVG

   Look at the spreadsheet cipher.ods, which you may open using the Calc
   spreadsheet program. The cell B1 contains the length of the key, and the
   cells B2:E2 contains the key chosen. The cells B4:AG4 contain the plaintext
   message.

   Complete this spreadsheet so that the desired ciphertext message
   "EDCAUXTWOLJZGWVPKJVGVGHQXWVPKJVG" appears in the ciphertext cells, B5:AG5.
   Assume that all characters are in uppercase letters between 'A' and 'Z'. You
   may use any other part of the spreadsheet for any purpose.

   You may use the tool available on the website
   https://www.dcode.fr/vigenere-cipher to test your implementation and to
   confirm encodings and decodings. *)
