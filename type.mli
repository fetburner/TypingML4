(* 型を表す型 *)
type t
exception Unify of t * t
(* 式に型を付ける *)
val of_Exp : Exp.t -> t
(* 型を文字列で表現する *)
val to_string : t -> string
