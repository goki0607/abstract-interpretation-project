(** file abstractDomainInterval.ml
  * implemented by Goktug Saatcioglu *)

open AbstractSyntaxExpressions

type abstractInterval = int * int

let bottom = (max_int, min_int)
let top = (min_int, max_int)

let isbottom (l, h) = h < l

let istop (l, h) = l = min_int && h = max_int

let ileq (l1,h1) (l2,h2) =
  (isbottom (l1,h1)) || ((l2 <= l1) && (h1 <= h2))

let ijoin (l1,h1) (l2,h2) =
  ((min l1 l2), (max h1 h2))

let imeet (l1,h1) (l2,h2) =
  ((max l1 l2), (min h1 h2))

(* a + b < min_int *)
let is_sum_min_overflow a b =
  if (a < 0) && (b < 0) then a < min_int - b
  else false

(* a + b > max_int *)
let is_sum_max_overflow a b =
  if (a > 0) && (b > 0) then a > max_int - b
  else false

let isum (l1,h1) (l2,h2) =
  if (isbottom (l1,h1) || isbottom (l2,h2)) then bottom 
  else if (is_sum_max_overflow l1 l2) then bottom
  else if (is_sum_min_overflow h1 h2) then bottom 
  else
    let l' = if (is_sum_min_overflow l1 l2) then min_int
             else l1 + l2 in 
    let h' = if (is_sum_max_overflow h1 h2) then max_int 
             else h1 + h2 in 
    (l', h')

(* a - b < min_int *)
let is_min_min_overflow a b =
  if (a < 0) && (b > 0) then a < min_int + b
  else false

(* a - b > max_int *)
let is_min_max_overflow a b =
  if (a > 0) && (b < 0) then a > max_int + b
  else false

let imin (l1,h1) (l2,h2) =
  if (isbottom (l1,h1) || isbottom (l2,h2)) then bottom 
  else if (is_min_max_overflow l1 h2) then bottom
  else if (is_min_min_overflow h1 l2) then bottom 
  else
    let l' = if (is_min_min_overflow l1 h2) then min_int
             else l1 - h2 in 
    let h' = if (is_sum_max_overflow h1 l2) then max_int 
             else h1 - l2 in 
    (l', h')

let ineg (l,h) = (-h,-l)

let back_imin (a,b) (l1,h1) (l2,h2) =
  if (isbottom (a,b)) || (isbottom (l1,h1)) || (isbottom (l2,h2)) then bottom, bottom
  else
    let la = max l1 (if is_sum_min_overflow a l2 then min_int else a + l2) in 
    let ha = min h1 (if is_sum_max_overflow b h2 then max_int else b + h2) in
    let lb = max l2 (if is_min_min_overflow l1 b then min_int else l1 - b) in 
    let hb = min h2 (if is_min_max_overflow h1 a then max_int else h1 - a) in
    let p1 = if isbottom (la,ha) then bottom else (la,ha) in 
    let p2 = if isbottom (lb,hb) then bottom else (lb,hb) in
    p1,p2

let back_iplus (a,b) (l1,h1) (l2,h2) =
  if (isbottom (a,b)) || (isbottom (l1,h1)) || (isbottom (l2,h2)) then bottom, bottom
  else
    ineg (l2,h2) |> back_imin (a,b) (l1,h1)

let stringofinterval (l,h) =
  if isbottom (l,h) then "_|_"
  else if istop (l,h) then "T"
  else 
    let ls = if l = min_int then "-oo" else string_of_int l in 
    let lh = if h = max_int then "oo" else string_of_int h in
    "["^ls^", "^lh^"]"

(* environments are represented as a function of "x", "y" and "z" only *)
type t = string -> abstractInterval

let leq r1 r2 = (ileq (r1 "x") (r2 "x")) &&
                (ileq (r1 "y") (r2 "y")) &&
                (ileq (r1 "z") (r2 "z"))

let initialize vl = ()

let bot () = function x -> bottom

let meet r1 r2 = function x -> imeet (r1 x) (r2 x)

let join r1 r2 = function x -> ijoin (r1 x) (r2 x)

(* we implicitly assume all variables are TOP initialized *)
let initialP () = function x -> (min_int,max_int)

let rec evala a r =
  match a with
  | Num (n) -> (n,n)
  | Var (x) ->
      if (x="x") || (x="y") || (x="z") then (r x)
      else failwith "AbstractDomainIntervals : undeclared variable"
  | Plus (a1, a2) -> isum (evala a1 r) (evala a2 r)
  | Minus (a1, a2) -> imin (evala a1 r) (evala a2 r)

let assign x a r = function y -> if (r x) |> isbottom then bottom else if (x=y) then (evala a r) else (r y)

let smash p1 p2 r =
  if isbottom p1 || isbottom p2 then bot ()
  else r 

let rec back_evala a r chi =
  match a with
  | Num (n) ->
      let i = (n,n) in 
      if imeet i chi |> isbottom then bot ()
      else r 
  | Var (x) ->
      if (x="x") || (x="y") || (x="z")
      then
        if imeet (r x) chi |> isbottom then bot ()
        else (function y -> if (x=y) then (imeet (r x) chi) else (r y))
      else failwith "AbstractDomainIntervals : undeclared variable"
  | Plus (a1, a2) ->
      let chi1, chi2 = back_iplus (evala a1 r) (evala a2 r) chi in 
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)
  | Minus (a1, a2) ->
      let chi1, chi2 = back_imin (evala a1 r) (evala a2 r) chi in 
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)

let stringofaP r = "x=" ^ (stringofinterval (r "x")) ^ ", y=" ^ (stringofinterval (r "y")) ^ ", z=" ^ (stringofinterval (r "z"))

let back_lt (l1,h1) (l2,h2) =
  if (isbottom (l1,h1)) || (isbottom (l2,h2)) || (l1 >= h2) then bottom, bottom
  else
    let h' = min h1 (h2 - 1) in 
    let l' = max l2 (l1 + 1) in 
    (l1,h'), (l',h2)

let back_not_lt (l1,h1) (l2,h2) =
  if isbottom (l1,h1) || isbottom (l2,h2) then bottom, bottom
  else if (h1 < l2) then bottom, bottom 
  else 
    let l' = max l1 l2 in 
    let h' = min h1 h2 in 
    (l',h1), (l2,h')

let back_eq (l1,h1) (l2,h2) =
  let r = imeet (l1,h1) (l2,h2) in
  if isbottom r then bottom, bottom 
  else r, r

let back_gt (l1,h1) (l2,h2) =
  if isbottom (l1,h1) || isbottom (l2,h2) || (h1 <= l2) then bottom, bottom
  else
    let l' = max l1 (l2 + 1) in 
    let h' = min h2 (h1 - 1) in 
    (l',h1), (l2,h')

let back_not_gt (l1,h1) (l2,h2) =
  if isbottom (l1,h1) || isbottom (l2,h2) then bottom, bottom
  else if (l1 > h2) then bottom, bottom
  else
    let h' = min h1 h2 in 
    let l' = max l1 l2 in 
    (l1,h'), (l',h2)

(*let back_neq (l1,h1) (l2,h2) =
  if isbottom (l1,h1) || isbottom (l2,h2) then bottom, bottom
  else
    let r1, r2 = back_lt (l1,h1) (l2,h2) in 
    let r3, r4 = back_gt (l1,h1) (l2,h2) in
    let p1 =
      if isbottom r1 && isbottom r3 then bottom
      else if isbottom r1 then r3
      else if isbottom r3 then r1
      else imeet r1 r3
    in 
    let p2 =
      if isbottom r2 && isbottom r4 then bottom
      else if isbottom r2 then r4
      else if isbottom r4 then r2
      else imeet r2 r4
    in 
    if isbottom p1 || isbottom p2 then bottom, bottom
    else p1, p2*)

let back_neq (l1,h1) (l2,h2) =
  if isbottom (l1,h1) || isbottom (l2,h2) then bottom, bottom
  else
    let r1, r2 = back_lt (l1,h1) (l2,h2) in 
    let r3, r4 = back_gt (l1,h1) (l2,h2) in
    let p1 =
      if isbottom r1 then bottom
      else r1
    in 
    let p2 =
      if isbottom r3 then bottom
      else r3
    in 
    if isbottom p1 && isbottom p2 then bottom, bottom
    else if isbottom p1 then bottom, p2
    else if isbottom p2 then p1, bottom
    else p1, p2

let rec back_test b r =
  match b with 
  | Lt (a1, a2) ->
      let chi1, chi2 = back_lt (evala a1 r) (evala a2 r) in
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)
  | Eq (a1, a2) ->
      let chi1, chi2 = back_lt (evala a1 r) (evala a2 r) in
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)
  | Neq (a1, a2) ->
      let t1 =
        let chi1, chi2 = back_lt (evala a1 r) (evala a2 r) in
        meet (back_evala a1 r chi1) (back_evala a2 r chi2)
      in 
      let t2 =
        let chi1, chi2 = back_gt (evala a1 r) (evala a2 r) in
        meet (back_evala a1 r chi1) (back_evala a2 r chi2)
      in 
      join t1 t2
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
      let chi1, chi2 = back_not_lt (evala a1 r) (evala a2 r) in
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)
  | Eq (a1, a2) ->
      let t1 =
        let chi1, chi2 = back_lt (evala a1 r) (evala a2 r) in
        meet (back_evala a1 r chi1) (back_evala a2 r chi2)
      in 
      let t2 =
        let chi1, chi2 = back_gt (evala a1 r) (evala a2 r) in
        meet (back_evala a1 r chi1) (back_evala a2 r chi2)
      in 
      join t1 t2
  | Neq (a1, a2) ->
      let chi1, chi2 = back_eq (evala a1 r) (evala a2 r) in
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)
  | Gt (a1, a2) ->
      let chi1, chi2 = back_not_gt (evala a1 r) (evala a2 r) in
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)
  | Nand (b1, b2) ->
      meet (back_test b1 r) (back_test b2 r)

(*
let test b r = back_test b r

let nottest b r = back_nottest b r
*)

(* no threshold *)
let thresholds = [min_int; max_int]
(* threshold 32.11 *)
(*let thresholds = [min_int; -1; 0; 1; max_int]*)
(* threshold 32.12 *)
(*let thresholds = [min_int; 1001; max_int]*)
let rev_thresholds = List.rev thresholds

let rec find_threshold elem op xs =
  match xs with
  | [] -> failwith "Threshold fail shouldn't happen!"
  | hd :: _ when op hd elem -> hd
  | _ :: tl -> find_threshold elem op tl

(* intreval widening without thresholds *)
(*let widen_interval (l1,h1) (l2,h2) =
  if isbottom (l1,h1) then (l2,h2)
  else if isbottom (l2,h2) then (l1,h2)
  else
    let l' = if l2 < l1 then min_int else l1 in 
    let h' = if h2 > h1 then max_int else h1 in 
    l', h'*)

(* interval widening with thresholds *)
let widen_interval (l1,h1) (l2,h2) =
  if isbottom (l1,h1) then (l2,h2)
  else if isbottom (l2,h2) then (l1,h1)
  else
    let l' = if l2 < l1 then find_threshold l2 (<=) rev_thresholds
             else l1 in 
    let h' = if h2 > h1 then find_threshold h2 (>=) thresholds
             else h1 in 
    l', h'

let widen r1 r2 = function x -> widen_interval (r1 x) (r2 x)

let narrow_interval (l1,h1) (l2,h2) =
  if isbottom (l1,h1) then bottom
  else if isbottom (l2,h2) then bottom
  else
    let l' = if l1 = min_int then l2 else l1 in 
    let h' = if h1 = max_int then h2 else h1 in 
    l', h'

let narrow r1 r2 = function x -> narrow_interval (r1 x) (r2 x)

let rec iterate f a =
   let x = (narrow a (f a)) in
      if (leq a x) then a else iterate f x

let test b r = iterate (back_test b) r

let nottest b r = iterate (back_nottest b) r