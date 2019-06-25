(** file abstractDomainCongruence.ml
  * implemented by Goktug Saatcioglu *)

open AbstractSyntaxExpressions

type abstractCongruence =
  | BOT                     (*    _|_ *)
  | Congruence of int * int (* c + mZ | TOP is in here *)

let normalize c m = if m = 0 then (c, m) else (c mod (abs m), abs(m))

let gcd a b =
  let rec gcd a b =
    if b = 0 then a else gcd b (a mod b)
  in
  gcd (abs (a)) (abs (b))

let lcm a b =
  match a, b with
  | 0, _ -> 0
  | _, 0 -> 0
  | _ -> abs (a * b) / (gcd a b)

let cleq c1 c2 =
  match c1, c2 with
  | BOT, _ -> true
  | _, BOT -> false
  | cc, cc' when cc = cc' -> true
  | Congruence (c, m), Congruence (c', m') ->
      if m' = 0 then false
      else if m mod m' = 0 then ((c - c') mod m') = 0
      else false

let cjoin c1 c2 =
  match c1, c2 with
  | BOT, c -> c
  | c, BOT -> c
  | Congruence (c, m), Congruence (c', m') ->
      let r = abs (c - c') in
      let m'' = gcd m m' |> gcd r in
      let cn, mn = normalize c m'' in
      Congruence (cn, mn)

let cmeet c1 c2 =
  match c1, c2 with
  | BOT, _ -> BOT
  | _, BOT -> BOT
  | Congruence (c, m), Congruence (c', m') ->
      let g = gcd m m' in
      if g = 0 then
        if c = c' then Congruence (c, 0)
        else BOT 
      else
        let r = (abs (c - c')) mod g in
        if r = 0 then
          let m'' = lcm m m' in
          let c'' = c mod m'' in 
          let cn, mn = normalize c'' m'' in 
          Congruence (cn, mn)
        else BOT

let cneg c1 =
  match c1 with
  | BOT -> BOT
  | Congruence (c, m) -> Congruence (-c, m)

let csum c1 c2 =
  match c1, c2 with
  | BOT, _ -> BOT
  | _, BOT -> BOT
  | Congruence (c, m), Congruence (c', m') ->
      let c'' = c + c' in
      let m'' = gcd m m' in
      let cn, mn = normalize c'' m'' in 
      Congruence (cn, mn)

let cmin c1 c2 =
  cneg c2 |> csum c1

let back_cmin x y r =
  let x' = csum y r |> cmeet x in 
  let y' = csum x r |> cmeet y in 
  (x', y')

let back_cplus x y r =
 back_cmin x (cneg y) r

let stringofcongruence c1 =
  match c1 with
  | BOT -> "_|_"
  | Congruence (c, m) ->
      (string_of_int c)^"+"^(string_of_int m)^"Z"


(* environments are represented as a function of "x", "y" and "z" only *)
type t = string -> abstractCongruence

let leq r1 r2 = (cleq (r1 "x") (r2 "x")) &&
                (cleq (r1 "y") (r2 "y")) &&
                (cleq (r1 "z") (r2 "z"))

let initialize vl = ()

let bot () = function x -> BOT

let meet r1 r2 = function x -> cmeet (r1 x) (r2 x)

let join r1 r2 = function x -> cjoin (r1 x) (r2 x)

(* we implicitly assume all variables are 0 initialized *)
let initialP () = function x -> Congruence (0, 0)

let rec evala a r =
  match a with
  | Num (n) -> Congruence (n, 0)
  | Var (x) ->
      if (x="x") || (x="y") || (x="z") then (r x)
      else failwith "AbstractDomainCongruence : undeclared variable"
  | Plus (a1, a2) -> csum (evala a1 r) (evala a2 r)
  | Minus (a1, a2) -> cmin (evala a1 r) (evala a2 r)

let assign x a r = function y -> if (r x) = BOT then BOT else if (x=y) then (evala a r) else (r y)

let smash p1 p2 r = (match (p1,p2) with
   | BOT, _ -> bot ()
   | _, BOT -> bot ()
   | _, _ -> r)

let rec back_evala a r chi =
  match a with
  | Num (n) ->
      let c = Congruence (n, 0) in 
      if cmeet c chi <> BOT then r
      else bot () 
  | Var (x) ->
      if (x="x") || (x="y") || (x="z")
      then
        if (cmeet (r x) chi) <> BOT
        then (function y -> if (x=y) then (cmeet (r x) chi) else (r y))
        else bot ()
      else failwith "AbstractDomainCongruence : undeclared variable"
  | Plus (a1, a2) ->
      let chi1, chi2 = back_cplus (evala a1 r) (evala a2 r) chi in 
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)
  | Minus (a1, a2) ->
      let chi1, chi2 = back_cmin (evala a1 r) (evala a2 r) chi in 
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)

let stringofaP r = "x=" ^ (stringofcongruence (r "x")) ^ ", y=" ^ (stringofcongruence (r "y")) ^ ", z=" ^ (stringofcongruence (r "z"))

let rec test b r =
  match b with
    | Lt (a1, a2) ->
        (match (evala a1 r), (evala a2 r) with
        | BOT, _ -> bot ()
        | _, BOT -> bot ()
        | Congruence (c1, 0), Congruence (c2, 0) ->
            if c1 < c2 then r else bot ()
        | _ -> r)
    | Eq (a1, a2) ->
        (match (evala a1 r), (evala a2 r) with
        | BOT, _ -> bot ()
        | _, BOT -> bot ()
        | Congruence (c1, 0), Congruence(c2, 0) ->
            if c1 = c2 then r else bot ()
        | _ -> r)
    | Neq (a1, a2) ->
        (match (evala a1 r), (evala a2 r) with
        | BOT, _ -> bot ()
        | _, BOT -> bot ()
        | Congruence (c1, 0), Congruence (c2, 0) ->
            if c1 != c2 then r else bot ()
        | _ -> r)
    | Gt (a1, a2) ->
        (match (evala a1 r), (evala a2 r) with
        | BOT, _ -> bot ()
        | _, BOT -> bot ()
        | Congruence (c1, 0), Congruence (c2, 0) ->
            if c1 > c2 then r else bot ()
        | _ -> r)
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
        | Congruence (c1, 0), Congruence (c2, 0) ->
            if c1 >= c2 then r else bot ()
        | _ -> r)
    | Eq (a1, a2) ->
        (match (evala a1 r), (evala a2 r) with
        | BOT, _ -> bot ()
        | _, BOT -> bot ()
        | Congruence (c1, 0), Congruence(c2, 0) ->
            if c1 != c2 then r else bot ()
        | _ -> r)
    | Neq (a1, a2) ->
        (match (evala a1 r), (evala a2 r) with
        | BOT, _ -> bot ()
        | _, BOT -> bot ()
        | Congruence (c1, 0), Congruence (c2, 0) ->
            if c1 = c2 then r else bot ()
        | _ -> r)
    | Gt (a1, a2) ->
        (match (evala a1 r), (evala a2 r) with
        | BOT, _ -> bot ()
        | _, BOT -> bot ()
        | Congruence (c1, 0), Congruence (c2, 0) ->
            if c1 <= c2 then r else bot ()
        | _ -> r)
    | Nand (b1, b2) ->
        meet (test b1 r) (test b2 r)

let widen_congruence c1 c2 = c2

let widen r1 r2 = function x -> widen_congruence (r1 x) (r2 x)

let narrow_congruence c1 c2 =
  match c1, c2 with
  | BOT, _ -> BOT
  | _, BOT -> BOT
  | Congruence (0,1), _ -> c2
  | _ -> c1

let narrow r1 r2 = function x -> narrow_congruence (r1 x) (r2 x)
        