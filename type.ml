(* 型環境 *)
module Env = Map.Make (String)

type t =
  | Int
  | Bool
  (* 型変数 *)
  | Var of t option ref
  | Fun of t * t
  | List of t

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
    | Fun (Fun _ as t1, t2) -> "(" ^ to_string_aux t1 ^ ") -> " ^ to_string_aux t2
    | Fun (t1, t2) -> to_string_aux t1 ^ " -> " ^ to_string_aux t2
    | List (Fun _ as t) -> "(" ^ to_string_aux t ^ ") list"
    | List (t) -> to_string_aux t ^ " list" in
  counter := 1;
  dic := [];
  to_string_aux t

(* 方程式に解が存在するか確かめる *)
let rec occur var = function
  | Var (var') when var == var' -> true
  | Int | Bool | Var { contents = None } -> false
  | Fun (t1, t2) -> occur var t1 || occur var t2
  | Var { contents = Some (t) } | List (t) -> occur var t

(* 型を単一化する *)
let rec unify t1 t2 =
  match t1, t2 with
  | Int, Int | Bool, Bool -> ()
  | Var { contents = Some (t1) }, t2 | t2, Var { contents = Some (t1) }
  | List (t1), List (t2) ->
      unify t1 t2
  | Fun (t11, t12), Fun (t21, t22) ->
      unify t11 t21;
      unify t12 t22
  | Var (var1), Var (var2) when var1 == var2 -> ()
  | Var ({ contents = None } as var), t2 | t2, Var ({ contents = None } as var) ->
      if occur var t2 then raise (Failure "recursive type")
      else var := Some (t2)

(* 与えられた型環境で式に型を付ける *)
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
      typing (Env.add x (typing env e1) env) e2
  | Exp.Fun (x, e) ->
      let alpha = Var (ref None) in 
      Fun (alpha, typing (Env.add x alpha env) e)
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
      unify t1 (List (alpha));
      unify t2 (typing (Env.add y t1 (Env.add x alpha env)) e3);
      t2

(* この型の表現ではある型tに対してtとVar { contents = Some (t) }の
   二通りができて気持ち悪かったので、前者に統一する関数を定義している *)
let rec flatten = function
  | Var { contents = Some (t) } -> flatten t
  | Fun (t1, t2) -> Fun (flatten t1, flatten t2)
  | List (t) -> List (flatten t)
  | t -> t

(* 関数の合成 *)
let ( >> ) f g x = g (f x)
let of_Exp = typing Env.empty >> flatten
