(* ML4の式を表す型 *)
type t =
  | Int of int
  | Bool of bool
  | Var of string
  | If of t * t * t
  | Let of string * t * t
  | Fun of string * t
  | App of t * t
  | LetRec of string * t * t
  | Nil
  | Cons of t * t
  | Match of t * t * string * string * t
