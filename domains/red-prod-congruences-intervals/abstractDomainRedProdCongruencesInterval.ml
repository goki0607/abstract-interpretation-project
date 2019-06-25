(** file abstractDomainRedProdCongruencesInterval.ml
  * implemented by Goktug Saatcioglu        *)

open AbstractSyntaxExpressions

module C = AbstractDomainCongruence
module I = AbstractDomainInterval

type abstractCongruenceInterval = 
  C.abstractCongruence * I.abstractInterval

let min c m a =
  if a = min_int then min_int
  else if m = 0 then failwith "AbstractDomainReducedProductCongruenceInterval : issue with min operator"
  else a + ((c-a) mod (abs m))

let max c m a =
  if a = max_int then max_int
  else if m = 0 then failwith "AbstractDomainReducedProductCongruenceInterval : issue with min operator"
  else a - ((a-c) mod (abs m))

let reduce (c,i) =
  match c,i with
  | C.BOT, _ -> C.BOT, I.bottom
  | _, i' when I.isbottom i' -> C.BOT, I.bottom
  | C.Congruence (n, 0), (l,h) ->
      if (l <= n && n <= h) then C.Congruence (n,0), (n,n)
      else C.BOT, I.bottom
  | C.Congruence (n, m) as c', (l,h) ->
      let mi, ma = min n m l, max n m h in 
      if mi > ma then C.BOT, I.bottom
      else if mi = ma then C.Congruence (mi, 0), (mi, mi)
      else c', (mi, ma) 

let rleq (c,i) (c',i') =
  C.cleq c c' && I.ileq i i'

let rjoin (c,i) (c',i') =
  (C.cjoin c c', I.ijoin i i') |> reduce

let rmeet (c,i) (c',i') =
  (C.cmeet c c', I.imeet i i') |> reduce

let rsum (c,i) (c',i') =
  (C.csum c c', I.isum i i') |> reduce

let rmin (c,i) (c',i') =
  (C.cmin c c', I.imin i i') |> reduce

let stringofred (c,i) =
  "("^(C.stringofcongruence c)^","^(I.stringofinterval i)^")"

(* environments are represented as a function of "x", "y" and "z" only *)
type t = string -> abstractCongruenceInterval

let leq r1 r2 = (rleq (r1 "x") (r2 "x")) &&
                (rleq (r1 "y") (r2 "y")) &&
                (rleq (r1 "z") (r2 "z"))

let initialize vl = ()

let bot () = function x -> C.BOT, I.bottom

let meet r1 r2 = function x -> rmeet (r1 x) (r2 x)

let join r1 r2 = function x -> rjoin (r1 x) (r2 x)

let initialP () = function x -> (C.Congruence(0, 0), I.top)

let ci n =
  C.Congruence (n,0), (n,n)

let rec evala a r =
  match a with
  | Num (n) -> ci n
  | Var (x) ->
      if (x="x") || (x="y") || (x="z") then (r x)
      else failwith "AbstractDomainReducedProductCongruenceInterval: : undeclared variable"
  | Plus (a1, a2) -> rsum (evala a1 r) (evala a2 r)
  | Minus (a1, a2) -> rmin (evala a1 r) (evala a2 r)

let isbottom (c,i) =
  (c = C.BOT) || (I.isbottom i)

let assign x a r =
  function y -> if (isbottom (r x)) then C.BOT, I.bottom else if (x=y) then (evala a r) else (r y)

let stringofaP r = "x=" ^ (stringofred (r "x")) ^ ", y=" ^ (stringofred (r "y")) ^ ", z=" ^ (stringofred (r "z"))

(* smash no longer needed *)
(*let smash p1 p2 r = (match (p1,p2) with
   | BOT, _ -> bot ()
   | _, BOT -> bot ()
   | _, _ -> r)*)

let back_plus (c,i) (c',i') (chi,chi') =
  let a, b = C.back_cplus c c' chi in 
  let c, d = I.back_iplus i i' chi' in 
  (reduce (a, c)), (reduce (b, d))

let back_minus (c,i) (c',i') (chi,chi') =
  let a, b = C.back_cmin c c' chi in 
  let c, d = I.back_imin i i' chi' in 
  (reduce (a, c)), (reduce (b, d))

let rec back_evala a r chi =
  match a with
  | Num (n) ->
      let cimeet = ci n |> rmeet chi in
      if isbottom cimeet then bot ()
      else r
  | Var (x) ->
      if (x="x") || (x="y") || (x="z")
      then
        let cimeet = rmeet (r x) chi in
        if isbottom cimeet then bot ()
        else 
          function y -> if (x=y) then (cimeet) else (r y)  
        (*then assign x (smeet (r x) chi) r*)
      else failwith "AbstractDomainReducedProductCongruenceInterval: : undeclared variable"
  | Plus (a1, a2) ->
      let chi1, chi2 = back_plus (evala a1 r) (evala a2 r) chi in 
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)
  | Minus (a1, a2) ->
      let chi1, chi2 = back_minus (evala a1 r) (evala a2 r) chi in 
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)

let back_lt (c,i) (c',i') =
  let a, b = 
    match c, c' with
    | C.BOT, _ -> C.BOT, C.BOT
    | _, C.BOT -> C.BOT, C.BOT
    | C.Congruence(c1, 0), C.Congruence(c2, 0) -> 
        if c1 < c2 then c, c' else C.BOT, C.BOT
    | _ -> c, c'
  in
  let c, d = I.back_lt i i' in 
  (a, c), (b, d) |> reduce

let back_eq (c,i) (c',i') =
  let a, b = 
    match c, c' with
    | C.BOT, _ -> C.BOT, C.BOT
    | _, C.BOT -> C.BOT, C.BOT
    | C.Congruence(c1, 0), C.Congruence(c2, 0) -> 
        if c1 = c2 then c, c' else C.BOT, C.BOT
    | _ -> c, c'
  in
  let c, d = I.back_eq i i' in 
  (a, c), (b, d) |> reduce

let back_neq (c,i) (c',i') =
  let a, b = 
    match c, c' with
    | C.BOT, _ -> C.BOT, C.BOT
    | _, C.BOT -> C.BOT, C.BOT
    | C.Congruence(c1, 0), C.Congruence(c2, 0) -> 
        if c1 <> c2 then c, c' else C.BOT, C.BOT
    | _ -> c, c'
  in
  let c, d = I.back_neq i i' in 
  (a, c), (b, d) |> reduce

let back_gt (c,i) (c',i') =
  let a, b = 
    match c, c' with
    | C.BOT, _ -> C.BOT, C.BOT
    | _, C.BOT -> C.BOT, C.BOT
    | C.Congruence(c1, 0), C.Congruence(c2, 0) -> 
        if c1 > c2 then c, c' else C.BOT, C.BOT
    | _ -> c, c'
  in
  let c, d = I.back_gt i i' in 
  (a, c), (b, d) |> reduce

let back_lte (c,i) (c',i') =
  let a, b = 
    match c, c' with
    | C.BOT, _ -> C.BOT, C.BOT
    | _, C.BOT -> C.BOT, C.BOT
    | C.Congruence(c1, 0), C.Congruence(c2, 0) -> 
        if c1 <= c2 then c, c' else C.BOT, C.BOT
    | _ -> c, c'
  in
  let c, d = I.back_not_lt i i' in 
  (a, c), (b, d) |> reduce

let back_gte (c,i) (c',i') =
  let a, b = 
    match c, c' with
    | C.BOT, _ -> C.BOT, C.BOT
    | _, C.BOT -> C.BOT, C.BOT
    | C.Congruence(c1, 0), C.Congruence(c2, 0) -> 
        if c1 >= c2 then c, c' else C.BOT, C.BOT
    | _ -> c, c'
  in
  let c, d = I.back_not_gt i i' in 
  (a, c), (b, d) |> reduce

let rec back_test b r =
  match b with 
  | Lt (a1, a2) ->
      let chi1, chi2 = back_lt (evala a1 r) (evala a2 r) in
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)
  | Eq (a1, a2) ->
      let chi1, chi2 = back_eq (evala a1 r) (evala a2 r) in
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)
  | Neq (a1, a2) ->
      let chi1, chi2 = back_neq (evala a1 r) (evala a2 r) in
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)
  | Gt (a1, a2) ->
      let chi1, chi2 = back_gt (evala a1 r) (evala a2 r) in
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)
  | Nand (b1, b2) ->
      let a = meet (back_nottest b1 r) (back_nottest b2 r) in
      let b = meet (back_nottest b1 r) (back_test b2 r) in
      let c = meet (back_test b1 r) (back_nottest b2 r) in
      join a b |> join c
and
back_nottest b r =
  match b with 
  | Lt (a1, a2) ->
      let chi1, chi2 = back_gte (evala a1 r) (evala a2 r) in
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)
  | Eq (a1, a2) ->
      let chi1, chi2 = back_neq (evala a1 r) (evala a2 r) in
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)
  | Neq (a1, a2) ->
      let chi1, chi2 = back_eq (evala a1 r) (evala a2 r) in
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)
  | Gt (a1, a2) ->
      let chi1, chi2 = back_lte (evala a1 r) (evala a2 r) in
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)
  | Nand (b1, b2) ->
      meet (back_test b1 r) (back_test b2 r)

let test b r = back_test b r

let nottest b r = back_nottest b r

let widen_red (c,i) (c',i') =
  C.widen_congruence c c', I.widen_interval i i'

let widen r1 r2 = function x -> widen_red (r1 x) (r2 x)

let narrow_red (c,i) (c',i') =
  C.narrow_congruence c c', I.narrow_interval i i'

let narrow r1 r2 = function x -> narrow_red (r1 x) (r2 x)