(** file abstractDomainConstancy.ml
  * implemented by Goktug Saatcioglu *)

open AbstractSyntaxExpressions

type abstractConstancy = BOT | Constant of int | TOP

let cleq c1 c2 =
  match c1, c2 with
  | BOT, _ -> true
  | _, BOT -> false
  | _, TOP -> true
  | TOP, _ -> false
  | Constant (a), Constant (b) -> a <= b

let cjoin c1 c2 =
  match c1, c2 with
  | BOT, c -> c
  | c, BOT -> c
  | _, TOP -> TOP
  | TOP, _ -> TOP
  | a, b -> if a = b then a else TOP

let cmeet c1 c2 =
  match c1, c2 with
  | BOT, _ -> BOT
  | _, BOT -> BOT
  | a, b ->
      if cleq a b then a else b

let cneg c1 =
  match c1 with
  | Constant (c) -> Constant (-c)
  | _ -> c1

let csum c1 c2 =
  match c1, c2 with
  | BOT, _ -> BOT
  | _, BOT -> BOT
  | _, TOP -> TOP
  | TOP, _ -> TOP
  | Constant (a), Constant (b) -> Constant (a+b)

let cmin c1 c2 =
  cneg c2 |> csum c1

let stringofconstancy c =
    match c with
    | BOT -> "_|_"
    | Constant (c') -> string_of_int c'
    | TOP -> "T"

(* environments are represented as a function of "x", "y" and "z" only *)
type t = string -> abstractConstancy

let leq r1 r2 = (cleq (r1 "x") (r2 "x")) &&
                (cleq (r1 "y") (r2 "y")) &&
                (cleq (r1 "z") (r2 "z"))

let initialize vl = ()

let bot () = function x -> BOT

let top () = function x -> TOP

let join r1 r2 = function x -> cjoin (r1 x) (r2 x)

let meet r1 r2 = function x -> cmeet (r1 x) (r2 x)

(* we implicitly assume all variables are 0 initialized *)
let initialP () = function x -> Constant (0)

let rec evala a r =
  match a with
  | Num (n) -> Constant (n)
  | Var (x) ->
      if (x="x") || (x="y") || (x="z") then (r x)
      else failwith "AbstractDomainConstancy : undeclared variable"
  | Plus (a1, a2) -> csum (evala a1 r) (evala a2 r)
  | Minus (a1, a2) -> cmin (evala a1 r) (evala a2 r)

let assign x a r = function y -> if (r x) = BOT then BOT else if (x=y) then (evala a r) else (r y)

let smash p1 p2 r = (match (p1,p2) with
   | BOT, _ -> bot ()
   | _, BOT -> bot ()
   | _, _ -> r)

let stringofaP r = "x=" ^ (stringofconstancy (r "x")) ^ ", y=" ^ (stringofconstancy (r "y")) ^ ", z=" ^ (stringofconstancy (r "z"))

let rec test b r =
  match b with
    | Lt (a1, a2) ->
        (match (evala a1 r), (evala a2 r) with
        | BOT, _ -> bot ()
        | _, BOT -> bot ()
        | Constant (c1), Constant (c2) ->
            if c1 < c2 then r else bot ()
        | _ -> r )
    | Eq (a1, a2) ->
        (match (evala a1 r), (evala a2 r) with
        | BOT, _ -> bot ()
        | _, BOT -> bot ()
        | Constant (c1), Constant (c2)->
            if c1 = c2 then r else bot ()
        | _ -> r )
    | Neq (a1, a2) ->
        (match (evala a1 r), (evala a2 r) with
        | BOT, _ -> bot ()
        | _, BOT -> bot ()
        | Constant (c1), Constant (c2) ->
            if c1 != c2 then r else bot ()
        | _ -> r )
    | Gt (a1, a2) ->
        (match (evala a1 r), (evala a2 r) with
        | BOT, _ -> bot ()
        | _, BOT -> bot ()
        | Constant (c1), Constant (c2) ->
            if c1 > c2 then r else bot ()
        | _ -> r )
    | Nand (b1, b2) ->
        let a = meet (nottest b1 r) (nottest b2 r) in
        let b = meet (nottest b1 r) (test b2 r) in
        let c = meet (test b1 r) (nottest b2 r) in
        join a b |> join c
and
nottest b r =
  match b with
    | Lt (a1, a2) ->
        (match (evala a1 r), (evala a2 r) with
        | BOT, _ -> bot ()
        | _, BOT -> bot ()
        | Constant (c1), Constant (c2) ->
            if c1 >= c2 then r else bot ()
        | _ -> r )
    | Eq (a1, a2) ->
        (match (evala a1 r), (evala a2 r) with
        | BOT, _ -> bot ()
        | _, BOT -> bot ()
        | Constant (c1), Constant (c2)->
            if c1 != c2 then r else bot ()
        | _ -> r )
    | Neq (a1, a2) ->
        (match (evala a1 r), (evala a2 r) with
        | BOT, _ -> bot ()
        | _, BOT -> bot ()
        | Constant (c1), Constant (c2) ->
            if c1 = c2 then r else bot ()
        | _ -> r )
    | Gt (a1, a2) ->
        (match (evala a1 r), (evala a2 r) with
        | BOT, _ -> bot ()
        | _, BOT -> bot ()
        | Constant (c1), Constant (c2) ->
            if c1 <= c2 then r else bot ()
        | _ -> r )
    | Nand (b1, b2) ->
        meet (test b1 r) (test b2 r)

let widen_constancy c1 c2 = c2

let widen r1 r2 = function x -> widen_constancy (r1 x) (r2 x)

let narrow_constancy c1 c2 = c2

let narrow r1 r2 = function x -> narrow_constancy (r1 x) (r2 x)
