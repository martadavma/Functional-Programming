(******************************************************************************)
(* Imp: *)

type aexp =
  | Int of int
  | Var of string
  | Plus of aexp * aexp
  | Minus of aexp * aexp

type bexp =
  | Bool of bool
  | Lt of aexp * aexp
  | Leq of aexp * aexp
  | Eq of aexp * aexp
  | And of bexp * bexp
  | Or of bexp * bexp
  | Not of bexp

type cmd =
  | Output of aexp
  | Asgn of string * aexp
  | Skip (* This command is the program which does nothing at all. *)
  | Seq of cmd * cmd
  | IfElse of bexp * cmd * cmd
  | While of bexp * cmd

(******************************************************************************)
(* Pretty printers: *)

let rec string_of_aexp (a : aexp) : string =
  match a with
  | Int c -> Int.to_string c
  | Var v -> v
  | Plus(a1, a2) -> "(" ^ (string_of_aexp a1) ^ ") + " ^
                    "(" ^ (string_of_aexp a2) ^ ")"
  | Minus(a1, a2) -> "(" ^ (string_of_aexp a1) ^ ") - " ^
                     "(" ^ (string_of_aexp a2) ^ ")"

let rec string_of_bexp (e : bexp) : string =
  match e with
  | Bool c -> Bool.to_string c
  | Lt(a1, a2) -> "(" ^ (string_of_aexp a1) ^ ") < " ^
                  "(" ^ (string_of_aexp a2) ^ ")"
  | Leq(a1, a2) -> "(" ^ (string_of_aexp a1) ^ ") <= " ^
                   "(" ^ (string_of_aexp a2) ^ ")"
  | Eq(a1, a2) -> "(" ^ (string_of_aexp a1) ^ ") == " ^
                  "(" ^ (string_of_aexp a2) ^ ")"
  | And(b1, b2) -> "(" ^ (string_of_bexp b1) ^ ") && " ^
                   "(" ^ (string_of_bexp b2) ^ ")"
  | Or(b1, b2) -> "(" ^ (string_of_bexp b1) ^ ") || " ^
                  "(" ^ (string_of_bexp b2) ^ ")"
  | Not b1 -> "!(" ^ (string_of_bexp b1) ^ ")"

let rec string_of_cmd (c : cmd) : string =
  let rec _string_of_cmd (c : cmd) (d : string) =
    match c with
    | Output a -> d ^ "output " ^ string_of_aexp a
    | Asgn(x, a) -> d ^ x ^ " = " ^ string_of_aexp a
    | Skip -> d ^ "skip"
    | Seq(c1, c2) -> _string_of_cmd c1 d ^ ";\n" ^
                     _string_of_cmd c2 d
    | IfElse(b, c1, c2) -> d ^ "if " ^ string_of_bexp b ^ " then\n" ^
                           _string_of_cmd c1 (d ^ "  ") ^ "\n" ^
                           d ^ "else\n" ^
                           _string_of_cmd c2 (d ^ "  ") ^ "\n" ^
                           d ^ "fi"
    | While(b, c1) -> d ^ "while " ^ string_of_bexp b ^ " do\n" ^
                      _string_of_cmd c1 (d ^ "  ") ^ "\n" ^
                      d ^ "done" in
  _string_of_cmd c ""

(******************************************************************************)
(* Some helper functions: *)

module Pmap =
  struct
    type ('a, 'b) t = ('a, 'b) Base.Map.Poly.t

    let empty : ('a, 'b) t = Base.Map.Poly.empty
    let is_empty : ('a, 'b) t -> bool = Base.Map.Poly.is_empty

    let add (key : 'a) (value : 'b) (map : ('a, 'b) t) : ('a, 'b) t =
      Base.Map.Poly.set map ~key:key ~data:value
    let find (key : 'a) (map : ('a, 'b) t) : 'b option =
      Base.Map.Poly.find map key

    let of_alist_multi (l : ('a * 'b) list) : ('a, 'b list) t =
      Base.Map.Poly.of_alist_multi l
    let to_alist (map : ('a, 'b) t) : ('a * 'b) list =
      Base.Map.Poly.to_alist ~key_order:`Increasing map
    let map (f : 'b -> 'c) (map : ('a, 'b) t) : ('a, 'c) t =
      Base.Map.Poly.map map ~f:f
  end

module Pset =
  struct
    type 'a t = 'a Base.Set.Poly.t
    let empty : 'a t = Base.Set.Poly.empty
    let contains : 'a t -> 'a -> bool = Base.Set.Poly.mem
    let add : 'a t -> 'a -> 'a t = Base.Set.Poly.add
  end
