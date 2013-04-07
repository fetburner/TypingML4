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

let to_string t =
  let counter = ref 1 in
  let dic = ref [] in
  let string_of_index n = String.make n '*' in
  let rec to_string_aux = function
    | Int -> "int"
    | Bool -> "bool"
    | Var ({ contents = None } as var) ->
        begin try List.assq var (!dic) with
        | Not_found ->
            let label = string_of_index (!counter) in
            dic := (var, label) :: !dic;
            counter := !counter + 1;
            label
        end
    | Var { contents = Some (t) } -> to_string_aux t
    | Fun ((Fun _) as t1, t2) -> "(" ^ to_string_aux t1 ^ ") -> " ^ to_string_aux t2
    | Fun (t1, t2) -> to_string_aux t1 ^ " -> " ^ to_string_aux t2
    | List ((Fun _) as t) -> "(" ^ to_string_aux t ^ ") list"
    | List (t) -> to_string_aux t ^ " list" in
  counter := 1;
  dic := [];
  to_string_aux t

let rec circulation var = function
  | Var (var') when var == var' -> true
  | Int | Bool | Var { contents = None } -> false
  | Fun (t1, t2) -> circulation var t1 || circulation var t2
  | Var { contents = Some (t) } | List (t) -> circulation var t

let rec unify t1 t2 =
  match t1, t2 with
  | Int, Int | Bool, Bool -> ()
  | Var ({ contents = None } as var), t | t, Var ({ contents = None } as var) ->
      if circulation var t then raise (Failure "recursive type")
      else var := Some (t)
  | Var { contents = Some (t1) }, t2 | t2, Var { contents = Some (t1) } ->
      unify t1 t2
  | Fun (t11, t12), Fun (t21, t22) ->
      unify t11 t21;
      unify t12 t22
  | List (t1), List (t2) -> unify t1 t2

let rec typing env = function
  | Exp.Int _ -> Int
  | Exp.Bool _ -> Bool
  | Exp.Var (x) -> Env.find x env
  | Exp.Plus (e1, e2) | Exp.Minus (e1, e2) | Exp.Times (e1, e2) -> 
      unify (typing env e1) Int;
      unify (typing env e2) Int;
      Int
  | Exp.Lt (e1, e2) -> 
      unify (typing env e1) Int;
      unify (typing env e2) Int;
      Bool
  | Exp.If (e1, e2, e3) -> 
      let t2 = typing env e2 in
      unify (typing env e1) Bool;
      unify t2 (typing env e3);
      t2
  | Exp.Let (x, e1, e2) -> 
      let t1 = typing env e1 in
      typing (Env.add x t1 env) e2
  | Exp.Fun (x, e) ->
      let alpha = Var (ref None) in 
      let t = typing (Env.add x alpha env) e in
      Fun (alpha, t)
  | Exp.App (e1, e2) ->
      let alpha = Var (ref None) in 
      unify (typing env e1) (Fun (typing env e2, alpha));
      alpha
  | Exp.LetRec (x, e1, e2) ->
      let alpha = Var (ref None) in
      let env' = Env.add x alpha env in 
      unify alpha (typing env' e1);
      typing env' e2
  | Exp.Nil ->
      List (Var (ref None))
  | Exp.Cons (e1, e2) -> 
      let t2 = typing env e2 in
      unify (List (typing env e1)) t2;
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
