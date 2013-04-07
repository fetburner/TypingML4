(* ML4の式を表す型 *)
type t =
  | Int of int
  | Bool of bool
  | Var of string
  | Plus of t * t
  | Minus of t * t
  | Times of t * t
  | Lt of t * t
  | If of t * t * t
  | Let of string * t * t
  | Fun of string * t
  | App of t * t
  | LetRec of string * t * t
  | Nil
  | Cons of t * t
  | Match of t * t * string * string * t
