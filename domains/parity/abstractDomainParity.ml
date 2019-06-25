(** file abstractDomainParity.ml  © P. Cousot 2018 
  * Changes made by Goktug Saatcioglu *)

open AbstractSyntaxExpressions

type abstractParity = BOT | ODD | EVEN | TOP

let pleq a1 a2 = 
  match a1, a2 with
  | BOT, _ -> true
  | _, BOT -> false
  | _, TOP -> true
  | TOP, _ -> false
  | a1, a2 -> a1=a2

let pjoin a1 a2 = 
  match a1, a2 with
  | BOT, a2 -> a2
  | a1, BOT -> a1
  | _, TOP -> TOP
  | TOP, _ -> TOP
  | a1, a2 -> if a1=a2 then a1 else TOP

let pmeet a1 a2 =
  match a1, a2 with
  | BOT, _ -> BOT
  | _, BOT -> BOT
  | a1, TOP -> a1
  | TOP, a2 -> a2
  | a1, a2 -> if a1=a2 then a1 else BOT

let psum a1 a2 = 
  match a1, a2 with
  | BOT, _ -> BOT
  | _, BOT -> BOT
  | ODD, EVEN -> ODD
  | EVEN, ODD -> ODD
  | EVEN, EVEN -> EVEN
  | ODD, ODD -> EVEN
  | _, _ -> TOP

let back_psum a1 a2 chi =
  match chi, a1, a2 with
  | TOP, _, _ -> a1, a2
  | EVEN, EVEN, EVEN -> EVEN, EVEN
  | EVEN, ODD, ODD -> ODD, ODD
  | ODD, EVEN, ODD -> EVEN, ODD
  | ODD, ODD, EVEN -> ODD, EVEN
  | _ -> BOT, BOT

let stringofparity a = 
  match a with
  | BOT -> "_|_"
  | ODD -> "o"
  | EVEN -> "e"
  | TOP -> "T"

(* environments are represented as a function of "x", "y" and "z" only *)
type t = string -> abstractParity

let leq r1 r2 = (pleq (r1 "x") (r2 "x")) &&
                (pleq (r1 "y") (r2 "y")) &&
                (pleq (r1 "z") (r2 "z"))

let initialize vl = ()

let bot () = function x -> BOT

let join r1 r2 = function x -> pjoin (r1 x) (r2 x)

let meet r1 r2 = function x -> pmeet (r1 x) (r2 x)

let initialP () = function x -> EVEN

let rec evala a r = 
  match a with
  | Num i -> if (i mod 2 = 0) then EVEN else ODD
  | Var x ->
      if (x="x") || (x="y") || (x="z") then (r x)
      else failwith "AbstractDomainParity : undeclared variable"
  | Minus (a1,a2) -> psum (evala a1 r) (evala a2 r)
  | Plus (a1,a2) -> psum (evala a1 r) (evala a2 r)

let rec back_evala a r chi =
  match a with
  | Num i ->
      let p = if (i mod 2 = 0) then EVEN else ODD in 
      if pmeet p chi <> BOT then r
      else bot ()
  | Var x ->
      if (x="x") || (x="y") || (x="z") then
        if (pmeet (r x) chi) <> BOT then r
        else bot ()
      else failwith "AbstractDomainParity : undeclared variable"
  | Plus (a1, a2) ->
      let chi1, chi2 = back_psum (evala a1 r) (evala a2 r) chi in 
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)
  | Minus (a1, a2) ->
      let chi1, chi2 = back_psum (evala a1 r) (evala a2 r) chi in 
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)

(*let assign x a r = function y -> if (x=y) then (evala a r) else (r y)*)
let assign x a r = 
  function y -> if (r x) = BOT then BOT else if (x=y) then (evala a r) else (r y)

let smash p1 p2 r = 
  match (p1,p2) with
  | BOT, _ -> bot ()
  | _, BOT -> bot ()
  | _, _ -> r

let back_eq p1 p2 =
  match p1, p2 with
  | TOP, _ -> p2, p2
  | _, TOP -> p1, p1
  | _ -> if p1 = p2 then p1, p1 else BOT, BOT

let back_neq p1 p2 =
  match p1, p2 with
  | BOT, _ -> BOT, BOT
  | _, BOT -> BOT, BOT
  | TOP, TOP -> EVEN, ODD
  | TOP, _ -> if p2 = EVEN then ODD, p2 else EVEN, p2
  | _, TOP -> if p1 = EVEN then p1, ODD else p1, EVEN
  | _ -> if p1 = p2 then BOT, BOT else p1, p2

let rec back_test b r =
  match b with 
  | Lt (a1, a2) -> smash (evala a1 r) (evala a2 r) r
  | Eq (a1, a2) ->
      let chi1, chi2 = back_eq (evala a1 r) (evala a2 r) in
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)
  | Neq (a1, a2) ->
      let chi1, chi2 = back_neq (evala a1 r) (evala a2 r) in
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)
  | Gt (a1, a2) -> smash (evala a1 r) (evala a2 r) r
  | Nand (b1, b2) ->
      let a = meet (back_nottest b1 r) (back_nottest b2 r) in
      let b = meet (back_nottest b1 r) (back_test b2 r) in
      let c = meet (back_test b1 r) (back_nottest b2 r) in
      join a b |> join c
and
back_nottest b r =
  match b with 
  | Lt (a1, a2) -> smash (evala a1 r) (evala a2 r) r
  | Eq (a1, a2) ->
      let chi1, chi2 = back_neq (evala a1 r) (evala a2 r) in
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)
  | Neq (a1, a2) ->
      let chi1, chi2 = back_eq (evala a1 r) (evala a2 r) in
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)
  | Gt (a1, a2) -> smash (evala a1 r) (evala a2 r) r
  | Nand (b1, b2) ->
      meet (back_test b1 r) (back_test b2 r)

let test b r = back_test b r

let nottest b r = back_nottest b r

let stringofaP r = "x=" ^ (stringofparity (r "x")) ^ ", y=" ^ (stringofparity (r "y")) ^ ", z=" ^ (stringofparity (r "z"))

let widen_parity p1 p2 = p2

let widen r1 r2 = function x -> widen_parity (r1 x) (r2 x)

let narrow_parity p1 p2 = p2

let narrow r1 r2 = function x -> narrow_parity (r1 x) (r2 x)
