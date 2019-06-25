(** file abstractDomainParity.ml  © P. Cousot 2018 
  * Changes made by Goktug Saatcioglu
  * to obtain abstractDomainTop.ml *)

open AbstractSyntaxExpressions

type abstractTop = BOT | TOP

let tleq a1 a2 = true

let tjoin a1 a2 = TOP

let tsum a1 a2 = TOP

let stringoftop a = "T"

(* environments are represented as a function of "x", "y" and "z" only *)
type t = string -> abstractTop
let leq r1 r2 = (tleq (r1 "x") (r2 "x")) &&
                (tleq (r1 "y") (r2 "y")) &&
                (tleq (r1 "z") (r2 "z"))
let initialize vl = ()
let top () = function x -> TOP
let bot () = function x -> BOT
let join r1 r2 = function x -> tjoin (r1 x) (r2 x)
let initialP () = function x -> TOP
let rec evala a r = match a with
   | Num i -> TOP
   | Var x -> if (x="x") || (x="y") || (x="z") then (r x)
              else failwith "AbstractDomainTop : undeclared variable"
   | Minus (a1,a2) -> tsum (evala a1 r) (evala a2 r)
   | Plus (a1,a2) -> tsum (evala a1 r) (evala a2 r)
let assign x a r = function y -> if (x=y) then (evala a r) else (r y)
let smash p1 p2 r = (match (p1,p2) with
   | TOP, _ -> top ()
   | _, TOP -> top ()
   | _, _ -> r)
let rec test b r = match b with 
   | Lt (a1,a2) -> smash (evala a1 r) (evala a2 r) r
   | Eq (a1,a2) -> (match (evala a1 r), (evala a2 r) with
                   | TOP, _ -> top ()
                   | _, TOP -> top ()
                   | _, _ -> r)
   | Neq (a1,a2) -> smash (evala a1 r) (evala a2 r) r
   | Gt (a1,a2) -> smash (evala a1 r) (evala a2 r) r
   | Nand (b1,b2) -> test b2 r (* coarse approximation *)
let nottest b r = match b with 
   | Lt (a1,a2) -> smash (evala a1 r) (evala a2 r) r
   | Eq (a1,a2) -> smash (evala a1 r) (evala a2 r) r
   | Neq (a1,a2) -> smash (evala a1 r) (evala a2 r) r
   | Gt (a1,a2) -> smash (evala a1 r) (evala a2 r) r
   | Nand (b1,b2) -> r (* coarse approximation *)
let stringofaP r = "x=" ^ (stringoftop (r "x")) ^ ", y=" ^ (stringoftop (r "y")) ^ ", z=" ^ (stringoftop (r "z"))
let widen_top a1 a2 = a2
let widen r1 r2 = function x -> widen_top (r1 x) (r2 x)
let narrow_top a1 a2 = a2
let narrow r1 r2 = function x -> narrow_top (r1 x) (r2 x)