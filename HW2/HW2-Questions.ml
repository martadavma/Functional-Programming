(*******************************************************************************
Homework 2
==========
CSCI 499, Fall 2022: Introduction to Functional Programming
Mukund Raghothaman
Due: 10pm PT on 11 October, 2022
*******************************************************************************)

(*******************************************************************************
- Your name: __________
- Your partner's name (if any): __________
*******************************************************************************)

(*******************************************************************************
Instructions
------------
1. Please setup your programming environment by following the steps outlined in
   the cheatsheet and on the course website.
2. Please feel free to collaborate with a partner, but make sure to write your
   submission individually.
3. Rename this file to HW2-yourname.ml, where yourname is your name, before
   submitting it on Blackboard.
4. Make sure that I can cleanly import this file into the OCaml toplevel when I
   call #use "HW2-yourname.ml". Comment out any questions you were unable to
   solve / have left unanswered.
*******************************************************************************)

exception NotImplemented

(** Distribution of points: 10 + 10 + 10 + 10 + 10 + 10 = 60 *)

(******************************************************************************)
(* Question 1: Run-Length Encoding and Decoding (5 + 5 points)

   The run-length encoding is a simple lossless algorithm for data compression,
   and was sometimes used in fax machines. The idea is to encode a sequence of
   repeated data items by its length. For example, the sequence ['a'; 'a'; 'b';
   'c'; 'c'; 'c'; 'a'] as the sequence [('a', 2); ('b', 1); ('c', 3); ('a', 1)],
   indicating that the element 'a' occurs thrice, followed by a single
   occurrence of 'b', and then by three occurrences of 'c' and one occurrence of
   'a', in that order.

   Write two functions, encode : 'a list -> ('a * int) list, and decode :
   ('a * int) list -> 'a list, which apply run-length compression to a sequence
   of data items and recover them respectively. Among other properties, ensure
   that for all lists l, decode (encode l) = l. In other words, l |> encode |>
   decode = l.

   Explain your solution. *)

let encode (l : 'a list) : ('a * int) list =
  raise NotImplemented

let decode (l : ('a * int) list) : 'a list =
  raise NotImplemented

(******************************************************************************)
(* Question 2: Tail-Recursive Fold-Right (10 points)

   Recall the definitions of fold_left and fold_right: *)

let rec fold_left f acc l =
  match l with
  | [] -> acc
  | hd :: tl -> fold_left f (f acc hd) tl

let rec fold_right f l acc =
  match l with
  | [] -> acc
  | hd :: tl -> f hd (fold_right f tl acc)

(* As discussed in class, fold_left induces the left-to-right flow of data in
   the diagram of
   https://r-mukund.github.io/teaching/fa2022-csci499/pdf/l07.pdf, while
   fold_right induces the right-to-left flow of data.

   Now, observe that the reference implementation of fold_left is
   tail-recursive, while fold_right is not tail-recursive. Recall that a
   function is tail-recursive if every recursive (and mutually recursive) call
   is the last action while evaluating the function.

   Write a tail-recursive implementation of the fold_right function. Ensure that
   your implementation works as a drop-in replacement for the traditional
   fold_right implementation shown above. In particular, ensure that your
   solution has the same type as the reference implementation.

   Explain your solution. *)

let fold_right2 (f : 'a -> 'b -> 'b) (l : 'a list) (acc : 'b) : 'b =
  raise NotImplemented

(* HINT: The main difference between fold_left and fold_right is in the
   direction of data flow. The list reversal function (rev : 'a list -> 'a list)
   is another function which similarly swaps the order of data.
   1. Can you write an implementation of rev which is tail-recursive?
   2. Can you use rev as a building block for a tail-recursive fold_right? *)

(******************************************************************************)
(* Question 3: Toy Map Reduce (10 points)

   Google's Map-Reduce framework (Dean and Ghemawat, 2004) is a popular
   mechanism to structure computations over large amounts of data, and its
   architecture is directly inspired by the map and fold operations of
   functional programming languages. You can access the original paper from
   https://research.google/pubs/pub62/. In this question, we will build a toy
   version of the Map-Reduce framework to understand the programming
   abstractions that it provides.

   At its heart, the user specifies two functions, mapper : 'vin ->
   ('kmid * 'vmid) list, and reducer : 'kmid -> 'vmid list -> 'vout list.
   Intuitively, the mapper function processes an individual data item (of type
   'vin) to produce a sequence of intermediate key-value results. The framework
   concurrently applies the mapper to each data item, groups the sequence of
   intermediate key-value pairs by their keys, and applies the reducer to the
   values of each group. Finally, it flattens the results from each application
   of the reduce function to obtain a unified list of results.

   The original Map-Reduce paper uses the example of a frequency counter for
   words in text. Here, the elemental functions would be defined as follows: *)

let mapper w = [ (w, 1) ]

let reducer w counts = [ (w, List.fold_left counts ~init:0 ~f:(+)) ]

(* Here, the text consists of a list of words, [w1; w2; w3; ...; wn]. The
   framework concurrently processes each word, to obtain a set of elemental
   counts, { (w1, 1), (w2, 1), (w3, 1), ..., (wn, 1) }. The framework then
   groups the records for each word wi, and evaluates reducer wi [1; 1; 1; ...;
   1] to obtain its frequency of occurrence, [ (wi, fi) ] in the text. The
   framework accumulates the results from all reducers to provide a unified
   output to the user, [ (w1, f1), (w2, f2), ..., (wk, fk) ].

   Write a function framework which applies a user-provided mapper and reducer
   method to the given data. Explain your solution. We have provided the desired
   type signatures for reference.

   You might find the following polymorphic map useful: *)

module Pmap :
  sig
    type ('a, 'b) t
    val to_alist : ('a, 'b) t -> ('a * 'b) list
    val empty_map : ('a, 'b) t
    val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
    val find : 'a -> ('a, 'b) t -> 'b option
  end =
  struct
    type ('a, 'b) t = ('a * 'b) list
    let to_alist = Fun.id
    let empty_map = []
    let add key value map =
      (key, value) :: List.filter (fun (k2, v) -> key != k2) map
    let find = List.assoc_opt
  end

(* Here's a three sentence tutorial to using the Map data structure defined
   above:

   1. Write Pmap.empty_map to obtain the empty map.

   2. To add values to a map, write:

      let new_map = Pmap.add key value old_map

   3. To obtain a value from the map, write:

      let opt_val = Pmap.find key map *)

let map_reduce (mapper : 'vin -> ('kmid * 'vmid) list)
               (reducer : 'kmid -> 'vmid list -> 'vout list)
               (data : 'vin list) : 'vout list =
  raise NotImplemented

(* If your solution is correct, for the example mapper and reducer from the
   frequency counting example, the expression

   map_reduce mapper reducer [ "why"; "what"; "how"; "why" ]

   should evaluate to the list [ ("why", 2); ("what", 1), ("how", 1) ], ignoring
   ordering. *)

(******************************************************************************)
(* Question 4: Folding Trees (2 + 2 + 6 points)

   We can naturally generalize the fold operation to binary trees, as
   follows: *)

type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

let rec fold_tree (init : 'b) (f : 'a -> 'b -> 'b -> 'b) (t : 'a tree) : 'b =
  match t with
  | Leaf -> init
  | Node(a, tl, tr) -> f a (fold_tree init f tl) (fold_tree init f tr)

(* Use the fold_tree method to implement the following operations:
   - size: 'a tree -> int, which returns the size of the input tree. For
     example, size Leaf = 1, and size(Node("x", Leaf, Leaf)) = 3. *)

let size (t : 'a tree) : int = raise NotImplemented

(* - mirror : 'a tree -> 'a tree, which returns the mirror image of the input
     tree. For example,

     mirror Node("x", Leaf, Node("y", Leaf, Leaf)) =
       Node("x", Node("y", Leaf, Leaf), Leaf). *)

let mirror (t : 'a tree) : 'a tree = raise NotImplemented

(* - select : int -> int tree -> int option, such that select k t returns the
     k-th smallest element of the tree t, if it exists, and None otherwise. For
     example, select 2 Node(8, Node(4, Leaf, Leaf), Node(1, Leaf, Leaf)) =
     Some 4. *)

let select (k : int) (t : int tree) : int option = raise NotImplemented

(* Explain each of your solutions above. *)

(******************************************************************************)
(* Q5: Imperative State 1: The Counter (10 points)

   Define a function counter : unit -> int which evaluates to the number of
   times it has previously been invoked. For example, the first call to
   counter () should evaluate to 0, the second call to counter () should
   evaluate to 1, the third call should evaluate to 2, and so on. *) 

let counter : unit -> int = raise NotImplemented

(* Such a function might conceivably be useful to produce logical timestamps or
   unique identifiers. How does your solution work? *)

(******************************************************************************)
(* Q6: Imperative State 2: The Mutable Queue (10 points)

   Recall that lists and strings and most other traditional data structures in
   OCaml are _immutable_. In other words, when we write a :: l to cons an
   element a to the front of a list l, we create a new list a :: l, and leave
   the original list unchanged. Similarly, when we wrote

   let new_map = Map.add key value old_map

   in Question 3 above, we obtain a new map with the key -> value mapping. The
   old map is left unchanged.

   Occasionally, in-place updates might be necessary. In this question, we will
   implement a mutable queue. Similar to Question 5, the operations would
   directly change the underlying data structure.

   Implement a structure corresponding to the module signature given below. You
   are free to choose any implementation strategy you find convenient.

   The type 'a t describes the type of mutable queues. The type of the queue is
   parameterized by the type of its elements, 'a. The function newQueue produces
   a new queue. The function enqueue takes a new element and inserts it into the
   queue. Observe that the return type of the function is unit, so it has to
   directly modify the provided queue. The function dequeue checks if the queue
   is non-empty, and in that case, returns the value at the front of the queue.
   Once again, it has to update the queue in place. *)

module type MutableQueueSig =
  sig
    type 'a t
    val newQueue: unit -> 'a t
    val enqueue: 'a -> 'a t -> unit
    val dequeue: 'a t -> 'a option
  end

(* You may start work on your implementation by uncommenting the following
   snippet of code:

module MutableQueue : MutableQueueSig =
  struct
    type 'a t = (* Fill in your type definition here. *)
    let newQueue () : 'a t = raise NotImplemented
    let enqueue (a : 'a) (q: 'a t) : unit = raise NotImplemented
    let dequeue (q: 'a t) : 'a option = raise NotImplemented
  end *)

(* How does your approach work?

   If your solution is correct, then the following lines should execute without
   error:

let _ =
  let q = MutableQueue.newQueue () in
  assert (MutableQueue.dequeue q = None);
  MutableQueue.enqueue 1 q;
  MutableQueue.enqueue 2 q;
  assert (MutableQueue.dequeue q = Some(1));
  MutableQueue.enqueue 3 q;
  assert (MutableQueue.dequeue q = Some(2));
  assert (MutableQueue.dequeue q = Some(3));
  assert (MutableQueue.dequeue q = None) *)
