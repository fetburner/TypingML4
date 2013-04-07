module Env = Map.Make (String)

type t =
  | Int
  | Bool
  | Var of t option ref
  | Fun of t * t
  | List of t

let rec flatten = function
  | Var { contents = Some (t) } -> flatten t
  | Fun (t1, t2) -> Fun (flatten t1, flatten t2)
  | List (t) -> List (flatten t)
  | t -> t

let rec to_string = function
  | Int -> "int"
  | Bool -> "bool"
  | Var { contents = Some (t) } -> to_string t
  | Var { contents = None } -> "?"
  | Fun (t1, t2) ->
      begin match t1 with
      | Fun _ -> "(" ^ to_string t1 ^ ")"
      | _ -> to_string t1
      end ^ " -> " ^ to_string t2
  | List (t) -> to_string t ^ " list"

let rec unify t1 t2 =
  match t1, t2 with
  | Int, Int | Bool, Bool -> ()
  | Var ({ contents = None } as var), t | t, Var ({ contents = None } as var) ->
      var := Some (t)
  | Var { contents = Some (t1) }, t2 | t2, Var { contents = Some (t1) } ->
      unify t1 t2
  | Fun (t11, t12), Fun (t21, t22) ->
      unify t11 t21;
      unify t12 t22
  | List (t1), List (t2) ->
      unify t1 t2

let rec typing env = function
  | Exp.Int _ -> Int
  | Exp.Bool _ -> Bool
  | Exp.Var (x) -> Env.find x env
  | Exp.Plus (e1, e2) | Exp.Minus (e1, e2) | Exp.Times (e1, e2) -> 
      let t1 = typing env e1 in
      let t2 = typing env e2 in
      unify t1 Int;
      unify t2 Int;
      Int
  | Exp.Lt (e1, e2) -> 
      let t1 = typing env e1 in
      let t2 = typing env e2 in
      unify t1 Int;
      unify t2 Int;
      Bool
  | Exp.If (e1, e2, e3) -> 
      let t1 = typing env e1 in
      let t2 = typing env e2 in
      let t3 = typing env e3 in
      unify t1 Bool;
      unify t2 t3;
      t2
  | Exp.Let (x, e1, e2) -> 
      let t1 = typing env e1 in
      let t2 = typing (Env.add x t1 env) e2 in
      t2
  | Exp.Fun (x, e) ->
      let alpha = Var (ref None) in 
      let t = typing (Env.add x alpha env) e in
      Fun (alpha, t)
  | Exp.App (e1, e2) ->
      let alpha = Var (ref None) in 
      let t1 = typing env e1 in
      let t2 = typing env e2 in
      unify t1 (Fun (t2, alpha));
      alpha
  | Exp.LetRec (x, e1, e2) ->
      let alpha = Var (ref None) in
      let env' = Env.add x alpha env in 
      let t1 = typing env' e1 in
      let t2 = typing env' e2 in
      unify alpha t1;
      t1
  | Exp.Nil ->
      List (Var (ref None))
  | Exp.Cons (e1, e2) -> 
      let t1 = typing env e1 in
      let t2 = typing env e2 in
      unify (List (t1)) t2;
      t2
  | Exp.Match (e1, e2, x, y, e3) ->
      let alpha = Var (ref None) in 
      let t1 = typing env e1 in
      let t2 = typing env e2 in
      let t3 = typing (Env.add y t1 (Env.add x alpha env)) e3 in
      unify t1 (List (alpha));
      unify t2 t3;
      t2

let ( >> ) f g x = g (f x)
let of_Exp = typing Env.empty >> flatten
