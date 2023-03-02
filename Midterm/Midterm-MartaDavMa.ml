(*******************************************************************************
Midterm Exam
============
CSCI 499, Fall 2022: An Introduction to Functional Programming
Mukund Raghothaman
Due: 10pm, Monday, 17 October 2022
*******************************************************************************)

(*******************************************************************************
- Your name: Marta Davila Mateu
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
(*
   int list -> bool
   The function e1 tales a list (l). Then it compares it's length with the first
   element of the list. Since to compare two objects they need to be of the same type
   we can safely say that the list has to be an int list. The it returns true if 
   the length is smaller than the head, and false otherwise, so the return type is a
   boolean. This is why the function e1 is of type : int list -> bool
*)

let e2 = fun f l -> (f (List.hd l)) :: l
(*
   ('a -> 'a) -> 'a list -> 'a list
   The function e2 takes as input a function that and a list. This function takes as
   input a type, and retuns as output the same type. And this list is a list of objects
   of the same type. Then it calls the inputed function with the head of the list as 
   input, and the returned object is added to the head of the inputed list. The resulting
   list when the outpued object is added is returned as output of the whole function 
   e2. This is why the function e2 is of type 
   ('a -> 'a) (* input: function tht takes an object as input and returns an object 
   of the same type as output *)
   -> 'a list (* input: list of the same type as the input and output of the above function*)
   -> 'a list (* output: list of the same type as above *).
*)

let e3 = fun o f ->
  match o with
  | Some x -> (match f x with
               | Some v -> Some v
               | None -> None)
  | None -> None
(*
   'a option -> ('a -> 'b option) -> 'b option
   The function e3 takes as input an object and a function. The object is an option,
   since it has to match with Some x or None. The function takes as input an object
   of the above type (not an option object, only the object x itself), and return an 
   option object of a different type. We knoe it returns a type option object, becuase
   the output has to match with Some v or None. Then the whole function returns an
   option object of the type ouputed from the input function. We knoe this becuase
   it either returns a None or it returns Some v which is the output of the inputed
   function. Therefore the whole function works as 
   'a option (* input: Some object of type one, or None *)
   -> ('a -> 'b option) (* input: function that takes as input an object of type one
   and returns Some object of type two or None *)
   -> 'b option (* output: Some object of type two or None *).
*)

let e4 = fun l -> List.fold_left (@) [] l
(*
   'a list list -> 'a list
   The function e4 takes as input a list of lists of objects. Then it folds left
   the list of lists by concatinanting (@) all the lists, starting from the empty
   list ([]). The resulting list made of all the concatinated lists is the output.
   Therefore the function works as:
   'a list list (* input: list of lists of objects *)
   -> 'a list (* output: list of objects *)

*)

let e5 = fun f o1 o2 ->
  match o1, o2 with
  | Some x1, Some x2 -> Some (f x1 x2)
  | None, _ -> None
  | _, None -> None
(*
   ('a -> 'b -> 'c) -> 'a option -> 'b option -> 'c option
   The function e5 takes as input a function, an option object of type one, and an
   option object of type two (meaning that the objects inside the Some are different).
   The inputed funtion takes as input an object of type one and an object of type two
   (the two types from the Some input), and results in an objectec of type three, of
   different type to the two above. The output of the function is of type option
   object three, meaning that is either None or Some object of the type resulting from
   the output of the inputed fucntion.
   Therefore the function works as:
   ('a -> 'b -> 'c) (* input: function takes objects of type a and b, and outputs an
   object of type c. *)
   -> 'a option (* input: Some object of a two or None *)
   -> 'b option (* input: Some object of b two or None *)
   -> 'c option (* output: Some object of c two or None *).
*)



(******************************************************************************)
(* Question 2: Types 2 (5 points)

   Provide examples of expressions with each of the following types: *)

let e6 : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c =
  fun f pair ->
    f (fst pair) (snd pair)

let e7 : 'a list list -> 'a list =
  fun l ->
    List.fold_left (@) [] l

let e8 : 'a list -> 'a list list =
  fun l ->
    [l]

type ('a, 'b) either = Left of 'a | Right of 'b

let e9 : ('a * 'b) -> ('a, 'b) either =
  fun pair ->
    if (1) = (2) then
      Left(fst pair)
    else 
      Right(snd pair)

let e10 : ('a, 'b) either -> ('a -> 'b) -> 'b =
  fun e f ->
    match e with
    | Left(a) -> (f a)
    | Right(b) -> (b)



(******************************************************************************)
(* Question 3: Types 3 (5 points)

   Each of the following expressions is ill-typed. Can you explain why? Can you
   modify them so that they are well-typed? *)

(* let e11 = 2 * 3.14 *)
(*
   The function/operator "*" takes two ints as input, and outputs an int.
   The function/operator "*." takes two floats and returns a float.
   To modify as minimum as possible, we would just change the float 3.14 
   and transform it into an int 3.
   To have the same result (2 * 3.14) we would convert 2 into a float (2.)
   and the function * into a float multiplicator *.
*)
let e11 = 2 * 3
let e11b = 2. *. 3.14

(* let e12 = fun x -> x + Int.to_float x *)
(*
  Just as with the function/operator *, "+" is developed for ints, and 
  "+." is developed for floats. "Int.to_float" is a function that takes an int 
  and returns a float. Therefore we know that x is supposed to be an int.
  However, "Int.to_float" is not needed, since + works with ints, and x is
  an int already.
*)
let e12 = 
  fun x -> 
    x + x

(* let e13 = 
   fun l -> 
    l :: List.hd l *)
(*
   The fucntion :: takes a value of the type insde the list and a list, 
   in this order, and puts the value in front of the list. Then it returns 
   the resulting list.
   Therefore, in this case the order of the inputs of :: are inverted, as 
   the object should always go in front of the ::, and the list after the ::.
*)
let e13 = 
  fun l -> 
   (List.hd l) :: l
(*
   In case the programmer wanted to add the element in the back of the list, she
   could use @ instead. @ takes two list and concatinates them. However both
  inputs have to be a list, so "List.hd l" would need to be inside a list.
*)
let e13b = 
  fun l -> 
   l @ [List.hd l]

(* let e14 = fun f -> f f *)
(*
   The compiler is not able to define the shape of f. There are two options:
   a. If f is a function, which requires an input (f : 'a -> 'b), then when 
   the input is generated by "f f" the second f requires an input. So it could
   be f (f x) where x is any object/value...
   b. If f is a function that does not require an input, then the ouput can't be
   generated by "f f", it should only be f.
*)
let e14 = 
  fun f -> 
   f
let e14b = 
fun f -> 
  f (f 4)

(* let e15 = fun f x -> f x + f [ x ] *)
(*
   The compiler not able to define the input of f: 
     is it ('a -> int) or ('a list -> int)?
   The inputed function f either takes as input an object of type x, or a list 
   of objects of types x. f is being called once with the object x 
   as input, and once with a list with object x inside. *)
let e15 = 
  fun f x -> 
    f x + f x
let e15b = 
  fun f x -> 
    f [x] + f [x]



(******************************************************************************)
(* Question 4: Understanding the Mechanics of Evaluation (5 points)

   Predict the result of evaluating each of the following expressions. Justify
   each of your predictions with a sentence or two. *)

(*let e16 = let x = 3 in let x = 5 in x + x (*Commented becuase causes warning*)*)
(*
   e16 = 10
   This is becuase of the variable scope.
   when x + x is called, the systme has x = 5, since it first looks for the variable
   closer to it.
   The execution woud be like:
   x = 3
     x = 5
       x + x ---> 5 + 5 --> 10
*)

let e17 = let x = 3 in (let x = 5 in x) + x
(*
   e17 = 8
   This is becuase the parenthesis surrounding "(let x = 5 in x)", make the x only
   equal to 5 within those parenthesis.
   The execution would be:
   x = 3
   (x = 5 in x) + x ---> 5 + x ---> 5 + 3 ---> 8
*)

let e18 =
  let f x = let g y = x + y in g in
  let g1 = f 3 in
  let g2 = f 8 in
  (g1 6) + (g2 9)
(*
  e18 = 26
  To understand this one we will analyse the code line by line.
  let e18 =
    let f input: x = 
      ouput: (let g input: y = 
              ouput: x + y 
              in g) 
    in let g1 = f 3   -->  f(x= 3) 6 ---> g(y=6) = 3 + y ---> 3 + 6 = 9
    in let g2 = f 8   -->  f(x= 8) 9 ---> g(y=9) = 8 + y ---> 8 + 9 = 17
    in (g1 6) + (g2 9)  -->  9 + 17 = 26
*)

let e19 =
  let x = ref 3 in
  let f y = (!x) + y in
  let z = f 9 in
  x := !x + 2;
  z + f 2
(*
   e19 = 19
   x is a reference to an int that can be modified.
   f is a function that takes an input y and returns the current value in !x 
      added to y.
   z is an int defined as f 9 = 12, regardless of !x's modifications.

   Then, if we follow the order, this is what is happening:
   ref x = 3
   f(y) = !x + y
   z = 12
   ref x = 5
   z   --> 12 
   + f 2   --> 5 + 2 = 7
   ---> 12 + 7 = 19
*)

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
(*
   e20 = 47
   When functions g1 and g2 are created, two separate p references are created
   for each function.
   Therefore when w1 is created, it uses !p = 3 (from g1), so w1 = 3 + 2, and 
      !p from g1 is now = 5.
   However, when w2 is created, it uses !p = 9 (from g2), so w2 = 9 + 5, and 
      !p from g2 is now = 14.
   For w3, !p from g1 is = 5, so w3 = 5 + 3 = 8.
   For w4, !p from g2 is = 14, so w4 = 14 + 6 = 20.
   When adding all of them, 5 + 14 + 8 + 20 = 47
*)



(******************************************************************************)
(* Question 5: Factoring Numbers (10 points)

   Write a program that determines all prime factors of a number, with
   multiplicities. Ensure that the output is presented in sorted order. We
   present a few test cases to clarify this question and to help test your
   solutions.

   How does your solution work? *)

let factors (n : int) : int list =

  (* Keeps a lists of all the already seen prime numbers. This makes it easier
     to find new prime numbers when looking for the next multiplier. *)
  let primes = ref [2] in

  (* This function adds a value to the list stored in the reference primes and 
    returns true. It is used when a prime that was not in the list is found. *)
  let updatePrimes (num : int) : bool =
    primes := !primes @ [num];
    true
  in

  (* Given any number, returns true if it is a prime, and false if it is not. *)
  let isPrime (num : int) : bool =
    if (num <= (List.nth !primes ((List.length !primes) -1))) then
      (List.exists (fun a -> a = num) !primes)
    else if (List.for_all (fun a -> num mod a != 0) !primes) then
      updatePrimes num
    else false
  in

  (* Starting from i=2, it checks if i is a prime and if it is divisible. 
    If it is it adds i to the list which accumulates (acc) all the multiplicities.
    It does not increase i since a number can have be divided many times by the 
    same number.
    If it is not, it skips i and checks the same with i+1.
    This is done until i is bigger than n, but not bigger becuase a number can be a
    multiplicity of itself (i.e: 2 = [2]) *)
  let rec recursive (num : int) (i : int) (acc : int list) : int list =
    if i > n then acc
    else if (isPrime i) && (num mod i = 0) then
        recursive (num / i) i (acc @ [i])
    else recursive num (i+1) acc
  in
  
  (* To start the recursion, we first discard 0 and 1. *)
  match n with
  | 0 -> []
  | 1 -> []
  | t -> recursive t 2 [] 
  
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
    ie: f(x) = a_0 * x^0 + a_1 * x^1 + a_2 * x^2 + a_3 * x^3 + ...

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
  let rec helper (left : float list) (current : float list) (cnt : int): float list =
    match left with
    | [] -> current
    | hd :: tl -> helper tl (current @ [(float_of_int cnt) *. hd]) (cnt + 1)
  in
  match lf with
  | [] -> []
  | ihd :: itl -> helper itl [] 1

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
  let g (hd : 'a) (ac : 'b list) : 'b list =
    (f hd) :: ac
  in
  let acc = []
  in
  fold_right g l acc

(* Next, recall the definition of filter: *)

let rec filter (f : 'a -> bool) (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | hd :: tl -> if f hd then hd :: (filter f tl) else filter f tl

(* Present an equivalent implementation, once again using fold_right as a
   primitive: *)

let filter2 (f : 'a -> bool) (l : 'a list) : 'a list =
  let g (hd : 'a) (ac : 'a list) : 'a list =
     if f hd then (hd :: ac)
     else ac
  in
  let acc = []
  in
  fold_right g l acc

 (* TEST:
  let f a = int_of_float a;;
  # let l = [3.;4.;5.;6.];;
  map f l;;
  map2 f l;;

  let f a = if a > 5 then true else false;;
  let l = [4;6;4;6;4;6];;
  let l = [4;6;4;7;4;8];;

  filter f l;;
  filter2 f l;; *)
