(** file abstractDomainRedProdParitySigns.ml
  * implemented by Goktug Saatcioglu        *)

open AbstractSyntaxExpressions

module S = AbstractDomainSign
module P = AbstractDomainParity

type abstractSignParity = 
  S.abstractSign * P.abstractParity

let reduce (s,p) =
  match s,p with
  | S.BOT, _ -> S.BOT, P.BOT
  | _, P.BOT -> S.BOT, P.BOT
  | S.LE0, P.ODD -> S.L0, P.ODD
  | S.E0, P.TOP -> S.E0, P.EVEN
  | S.E0, P.ODD -> S.BOT, P.BOT
  | S.GE0, P.ODD -> S.G0, P.ODD 
  | _ -> s, p

let rleq (s,p) (s',p') =
  S.sleq s s' && P.pleq p p'

let rjoin (s,p) (s',p') =
  (S.sjoin s s', P.pjoin p p') |> reduce

let rmeet (s,p) (s',p') =
  (S.smeet s s', P.pmeet p p') |> reduce

let rsum (s,p) (s',p') =
  (S.ssum s s', P.psum p p') |> reduce

let rmin (s,p) (s',p') =
  (S.smin s s', P.psum p p') |> reduce

let stringofred (s,p) =
  "("^(S.stringofsign s)^","^(P.stringofparity p)^")"

(* environments are represented as a function of "x", "y" and "z" only *)
type t = string -> abstractSignParity

let leq r1 r2 = (rleq (r1 "x") (r2 "x")) &&
                (rleq (r1 "y") (r2 "y")) &&
                (rleq (r1 "z") (r2 "z"))

let initialize vl = ()

let bot () = function x -> S.BOT, P.BOT

let meet r1 r2 = function x -> rmeet (r1 x) (r2 x)

let join r1 r2 = function x -> rjoin (r1 x) (r2 x)

let initialP () = function x -> (S.E0, P.EVEN)

let sp n =
  let sign =
    if n < 0 then S.L0
    else if n > 0 then S.G0
    else S.E0
  in
  let parity = if (n mod 2 = 0) then P.EVEN else P.ODD in
  sign, parity

let rec evala a r =
  match a with
  | Num (n) -> sp n
  | Var (x) ->
      if (x="x") || (x="y") || (x="z") then (r x)
      else failwith "AbstractDomainReducedProductSignsParity : undeclared variable"
  | Plus (a1, a2) -> rsum (evala a1 r) (evala a2 r)
  | Minus (a1, a2) -> rmin (evala a1 r) (evala a2 r)

let isbottom (s,p) =
  (s = S.BOT) || (p = P.BOT)

let assign x a r =
  function y -> if (isbottom (r x)) then S.BOT, P.BOT else if (x=y) then (evala a r) else (r y)

let stringofaP r = "x" ^ (stringofred (r "x")) ^ ", y" ^ (stringofred (r "y")) ^ ", z" ^ (stringofred (r "z"))

(* smash no longer needed *)
(*let smash p1 p2 r = (match (p1,p2) with
   | BOT, _ -> bot ()
   | _, BOT -> bot ()
   | _, _ -> r)*)

let back_plus (s,p) (s',p') (chi,chi') =
  let a, b = S.back_plus s s' chi in 
  let c, d = P.back_psum p p' chi' in 
  (reduce (a, c)), (reduce (b, d))

let back_minus (s,p) (s',p') (chi,chi') =
  let a, b = S.back_minus s s' chi in 
  let c, d = P.back_psum p p' chi' in 
  (reduce (a, c)), (reduce (b, d))

let rec back_evala a r chi =
  match a with
  | Num (n) ->
      let spmeet = sp n |> rmeet chi in
      if isbottom spmeet then bot ()
      else r
  | Var (x) ->
      if (x="x") || (x="y") || (x="z")
      then
        let spmeet = rmeet (r x) chi in
        if isbottom spmeet then bot ()
        else 
          function y -> if (x=y) then (spmeet) else (r y)  
        (*then assign x (smeet (r x) chi) r*)
      else failwith "AbstractDomainReducedProductSignsParity : undeclared variable"
  | Plus (a1, a2) ->
      let chi1, chi2 = back_plus (evala a1 r) (evala a2 r) chi in 
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)
  | Minus (a1, a2) ->
      let chi1, chi2 = back_minus (evala a1 r) (evala a2 r) chi in 
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)

let back_lt (s,p) (s',p') =
  let a, b = S.back_lt s s' in
  (a, p), (b, p') |> reduce

let back_eq (s,p) (s',p') =
  let a, b = S.back_eq s s' in 
  let c, d = P.back_eq p p' in 
  (a, c), (b, d) |> reduce

let back_neq (s,p) (s',p') =
  let a, b = S.back_neq s s' in 
  let c, d = P.back_neq p p' in 
  (a, c), (b, d) |> reduce

let back_gt (s,p) (s',p') =
  let a, b = S.back_gt s s' in
  (a, p), (b, p') |> reduce

let back_lte (s,p) (s',p') =
  let a, b = S.back_lte s s' in
  (a, p), (b, p') |> reduce

let back_gte (s,p) (s',p') =
  let a, b = S.back_gte s s' in
  (a, p), (b, p') |> reduce

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

let widen_red (s,p) (s',p') =
  S.widen_sign s s', P.widen_parity p p'

let widen r1 r2 = function x -> widen_red (r1 x) (r2 x)

let narrow_red (s,p) (s',p') =
  S.narrow_sign s s', P.narrow_parity p p'

let narrow r1 r2 = function x -> narrow_red (r1 x) (r2 x)