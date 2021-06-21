let hello = "hello";;

type term =
  | Var of string
  | Abs of string * term
  | App of term * term
  | S | K | I
;;


let rec showTerm t =
  match t with
    | Var x		-> x
    | Abs (x,t) -> "\\" ^ x ^ "->" ^ showTerm t
    | App (t1,t2)	-> "(" ^ showTerm t1 ^ " " ^ showTerm t2 ^ ")"
    | S -> "S" | K -> "K" | I -> "I"

let rec isFree sx st =
match st with
| Var x	-> sx <> x
| Abs (x,t) -> isFree sx t
| App (t1,t2)	-> (isFree sx t1) && (isFree sx t2)
| combs -> true

let comb2Lamb st =
match st with
| S ->
  let left = App ((Var "f"),(Var "x")) in
  let right = App ((Var "g"),(Var "x")) in
  let inner = App (left, right) in
  let outer = Abs ("f",Abs("g",Abs ("x",inner))) in
  outer
| K -> Abs("x",Abs("y",Var "x"))
| I -> Abs("x",Var"x")
| lambs -> lambs
;;

let rec lamb2Comb st =
match st with
| Var x -> st 
| Abs(x,t) ->
  if isFree x t then
    let () = print_endline ("free:" ^ showTerm t) in
    App (K,lamb2Comb t)
  else (
    match t with
    | Var _			-> I (* x' must equal to x *)
    | Abs(_,_)	-> lamb2Comb (Abs(x, lamb2Comb t))
    | App(t1,t2) ->
      App(App(S,lamb2Comb (Abs(x,t1))),lamb2Comb (Abs(x,t2)))
    | combs -> combs
  )
| App(t1,t2)	->
  App(lamb2Comb t1, lamb2Comb t2)
| combs -> combs
;;

let rec subst sx st dt =
match dt with
| Var x	->
  if x = sx then st else dt
| Abs(x,t)	->
  if x = sx then dt
  else Abs(x,(subst sx st t))
| App(t1,t2) ->
  App((subst sx st t1),(subst sx st t2))
| combs -> combs
;;

module SS = Set.Make(String)
let rec freeSet t =
match t with
| Var x		-> SS.singleton(x)
| Abs (x,t) -> SS.remove x (freeSet t)
| App (t1,t2)	-> SS.union (freeSet t1) (freeSet t2)
| combs	-> SS.empty
;;

let rec isNormal t =
match t with
| Var _ -> true
| Abs(_,_) -> true
| App(Var _,_) -> true
| App(Abs(_,_),_) -> false
| App(App(App(S,f),g),x) -> false
| App(App(K,x),y) -> false
| App(I,x) -> false
| App(App(_,_) as t,_) -> isNormal t 
| combs -> true
;;

let rec newVar x t =
let rec loop i =
  if isFree (x ^ string_of_int i) t
    then (x ^ string_of_int i)
    else loop (i+1) in
loop 1
;;

let rec alpha xs dt =
match dt with
| Var _ -> dt
| Abs(x,t) ->
  if SS.mem x xs then
    let nv = newVar x t in
    alpha (SS.remove x xs) (Abs(nv, (subst x (Var nv) t)))
  else 
    Abs(x,alpha xs t)
| App(t1,t2) -> App(alpha xs t1, alpha xs t2)
| combs -> combs
;;

let rec eval t =
let () = print_endline ("DEBUG:" ^ showTerm t) in
match t with
| Var _ -> t
| Abs(_,_) -> t
| App(Var _, _) -> t
| App(Abs(x,t) as t1,t2) ->
  let result = subst x t2 t in
  let resSet = (freeSet result) in
  let argSet = (freeSet t2) in
  if SS.subset argSet resSet then result
    else eval (App(alpha (SS.diff argSet resSet) t1,t2))
| App(App(App(S,f),g),x) -> eval (App(App(f,x),App(g,x)))
| App(App(K,x),y) -> eval x
| App(I,x) -> eval x
| App(t1,t2) -> if isNormal t1 then t else eval (App(eval t1,t2))
| other -> other
;;

let swap = Abs("x",Abs("y",App(Var "y",Var "x")))
let t1 = Abs("y",App(Var "y",Var "x"))
let t2 = App(Var "y",Var "2")

let () = print_endline (if isFree "y" swap then "T" else "F")
let test = App(swap,t2)

let test2 = App(lamb2Comb swap,t2)

let () = begin
print_endline (showTerm test);
print_endline (showTerm (eval (lamb2Comb (eval test))));
print_endline (showTerm test2);
print_endline (showTerm (eval test2));
end
