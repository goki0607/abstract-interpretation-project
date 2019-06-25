(** file abstractDomainSigns.ml
  * implemented by Goktug Saatcioglu *)

open AbstractSyntaxExpressions

type abstractSign = 
  | BOT (*  _|_ *)
  | L0  (*  < 0 *)
  | E0  (*  = 0 *)
  | G0  (*  > 0 *)
  | LE0 (* <= 0 *)
  | N0  (* != 0 *)
  | GE0 (* >= 0 *)
  | TOP (*    T *)


let get_idx s =
  match s with
  | BOT -> 0
  | L0 -> 1
  | E0 -> 2
  | G0 -> 3
  | LE0 -> 4
  | N0 -> 5
  | GE0 -> 6
  | TOP -> 7

let choose t s1 s2 =
  get_idx s2 |> (get_idx s1 |> Array.get t |> Array.get)

let leq_table =
  (* table for leq : [= *)
  (*                BOT     L0     E0     G0    LE0     N0    GE0   TOP      *)
  (* BOT *) [|[|  true;  true;  true;  true;  true;  true;  true;  true |];
  (*  L0 *)   [| false;  true; false; false;  true;  true; false;  true |];
  (*  E0 *)   [| false; false;  true; false;  true; false;  true;  true |];
  (*  G0 *)   [| false; false; false;  true; false;  true;  true;  true |];
  (* LE0 *)   [| false; false; false; false;  true; false; false;  true |];
  (*  N0 *)   [| false; false; false; false; false;  true; false;  true |];
  (* GE0 *)   [| false; false; false; false; false; false;  true;  true |];
  (* TOP *)   [| false; false; false; false; false; false; false;  true |]|]

let sleq s1 s2 = choose leq_table s1 s2

let join_table =
  (* table for join : |_| *)
  (*               BOT     L0     E0     G0    LE0     N0    GE0    TOP      *)
  (* BOT *) [|[|   BOT;    L0;    E0;    G0;   LE0;    N0;   GE0;   TOP |];
  (*  L0 *)   [|    L0;    L0;   LE0;    N0;   LE0;    N0;   TOP;   TOP |];
  (*  E0 *)   [|    E0;   LE0;    E0;   GE0;   LE0;   TOP;   GE0;   TOP |];
  (*  G0 *)   [|    G0;    N0;   GE0;    G0;   TOP;    N0;   GE0;   TOP |];
  (* LE0 *)   [|   LE0;   LE0;   LE0;   TOP;   LE0;   TOP;   TOP;   TOP |];
  (*  N0 *)   [|    N0;    N0;   TOP;    N0;   TOP;    N0;   TOP;   TOP |];
  (* GE0 *)   [|   GE0;   TOP;   GE0;   GE0;   TOP;   TOP;   GE0;   TOP |];
  (* TOP *)   [|   TOP;   TOP;   TOP;   TOP;   TOP;   TOP;   TOP;   TOP |]|]

let sjoin s1 s2 = choose join_table s1 s2

let meet_table =
  (* table for join : |¯| *)
  (*               BOT     L0     E0     G0    LE0     N0    GE0    TOP      *)
  (* BOT *) [|[|   BOT;   BOT;   BOT;   BOT;   BOT;   BOT;   BOT;   BOT |];
  (*  L0 *)   [|   BOT;    L0;   BOT;   BOT;    L0;    L0;   BOT;    L0 |];
  (*  E0 *)   [|   BOT;   BOT;    E0;   BOT;    E0;   BOT;    E0;    E0 |];
  (*  G0 *)   [|   BOT;   BOT;   BOT;    G0;   BOT;    G0;    G0;    G0 |];
  (* LE0 *)   [|   BOT;    L0;    E0;   BOT;   LE0;    L0;    E0;   LE0 |];
  (*  N0 *)   [|   BOT;    L0;   BOT;    G0;    L0;    N0;    G0;    N0 |];
  (* GE0 *)   [|   BOT;   BOT;    E0;    G0;    E0;    G0;   GE0;   GE0 |];
  (* TOP *)   [|   BOT;    L0;    E0;    G0;   LE0;    N0;   GE0;   TOP |]|]

let smeet s1 s2 = choose meet_table s1 s2

let forward_sum_table =
  (* table for addition : + *)
  (*               BOT     L0     E0     G0    LE0     N0    GE0    TOP      *)
  (* BOT *) [|[|   BOT;   BOT;   BOT;   BOT;   BOT;   BOT;   BOT;   BOT |];
  (*  L0 *)   [|   BOT;    L0;    L0;   TOP;    L0;   TOP;   TOP;   TOP |];
  (*  E0 *)   [|   BOT;    L0;    E0;    G0;   LE0;    N0;   GE0;   TOP |];
  (*  G0 *)   [|   BOT;   TOP;    G0;    G0;   TOP;   TOP;    G0;   TOP |];
  (* LE0 *)   [|   BOT;    L0;   LE0;   TOP;   LE0;   TOP;   TOP;   TOP |];
  (*  N0 *)   [|   BOT;   TOP;    N0;   TOP;   TOP;   TOP;   TOP;   TOP |];
  (* GE0 *)   [|   BOT;   TOP;   GE0;   GE0;   TOP;   TOP;   GE0;   TOP |];
  (* TOP *)   [|   BOT;   TOP;   TOP;   TOP;   TOP;   TOP;   TOP;   TOP |]|]

let ssum s1 s2 = choose forward_sum_table s1 s2

let forward_min_table =
  (* table for subtraction : - *)
  (*               BOT     L0     E0     G0    LE0     N0    GE0    TOP      *)
  (* BOT *) [|[|   BOT;   BOT;   BOT;   BOT;   BOT;   BOT;   BOT;   BOT |];
  (*  L0 *)   [|   BOT;   TOP;    L0;    L0;   TOP;   TOP;    L0;   TOP |];
  (*  E0 *)   [|   BOT;    G0;    E0;    L0;   LE0;    N0;   GE0;   TOP |];
  (*  G0 *)   [|   BOT;    G0;    G0;   TOP;    G0;   TOP;   TOP;   TOP |];
  (* LE0 *)   [|   BOT;   TOP;   LE0;    L0;   TOP;   TOP;   LE0;   TOP |];
  (*  N0 *)   [|   BOT;   TOP;    N0;   TOP;   TOP;   TOP;   TOP;   TOP |];
  (* GE0 *)   [|   BOT;    G0;   GE0;   TOP;   GE0;   TOP;   TOP;   TOP |];
  (* TOP *)   [|   BOT;   TOP;   TOP;   TOP;   TOP;   TOP;   TOP;   TOP |]|]

let smin s1 s2 = choose forward_min_table s1 s2

let stringofsign s =
  match s with
  | BOT -> "=_|_"
  | L0 -> "<0"
  | E0 -> "=0"
  | G0 -> ">0"
  | LE0 -> "<=0"
  | N0 -> "!=0"
  | GE0 -> ">=0"
  | TOP -> "=T"

(* environments are represented as a function of "x", "y" and "z" only *)
type t = string -> abstractSign

let leq r1 r2 = (sleq (r1 "x") (r2 "x")) &&
                (sleq (r1 "y") (r2 "y")) &&
                (sleq (r1 "z") (r2 "z"))

let initialize vl = ()

let bot () = function x -> BOT

let meet r1 r2 = function x -> smeet (r1 x) (r2 x)

let join r1 r2 = function x -> sjoin (r1 x) (r2 x)

let initialP () = function x -> E0

let rec evala a r =
  match a with
  | Num (n) ->
      if n < 0 then L0
      else if n > 0 then G0
      else E0
  | Var (x) ->
      if (x="x") || (x="y") || (x="z") then (r x)
      else failwith "AbstractDomainSigns : undeclared variable"
  | Plus (a1, a2) -> ssum (evala a1 r) (evala a2 r)
  | Minus (a1, a2) -> smin (evala a1 r) (evala a2 r)
  
let assign x a r = function y -> if (r x) = BOT then BOT else if (x=y) then (evala a r) else (r y)

let smash p1 p2 r = (match (p1,p2) with
   | BOT, _ -> bot ()
   | _, BOT -> bot ()
   | _, _ -> r)

let lt_table =
  (* table for lt : < *)
  (*                BOT     L0     E0     G0    LE0     N0    GE0   TOP      *)
  (* BOT *) [|[| false; false; false; false; false; false; false; false |];
  (*  L0 *)   [| false;  true;  true;  true;  true;  true;  true;  true |];
  (*  E0 *)   [| false; false;  true;  true; false;  true;  true;  true |];
  (*  G0 *)   [| false; false; false;  true; false;  true;  true;  true |];
  (* LE0 *)   [| false;  true;  true;  true;  true;  true;  true;  true |];
  (*  N0 *)   [| false;  true;  true;  true;  true;  true;  true;  true |];
  (* GE0 *)   [| false; false; false;  true; false;  true;  true;  true |];
  (* TOP *)   [| false;  true;  true;  true;  true;  true;  true;  true |]|]

let lt s1 s2 = choose lt_table s1 s2

let eq s1 s2 =
  match (smeet s1 s2) with
  | BOT -> false
  | _ -> true

let gt s1 s2 =
  (not (lt s1 s2)) || (eq s1 s2)

let neq s1 s2 = eq s1 s2 |> not

let stringofaP r = "x" ^ (stringofsign (r "x")) ^ ", y" ^ (stringofsign (r "y")) ^ ", z" ^ (stringofsign (r "z"))

let neg chi =
  match chi with
  | BOT -> BOT
  | L0 -> GE0
  | E0 -> E0
  | G0 -> LE0
  | LE0 -> G0
  | N0 -> N0
  | GE0 -> L0
  | TOP -> TOP

let l0_p_table =
  (* table for chi1 + chi1 = L0 : [= *)
  (*                   BOT        L0        E0        G0       LE0        N0       GE0      TOP      *)
  (* BOT *) [|[|(BOT,BOT);(BOT,BOT);(BOT,BOT);(BOT,BOT);(BOT,BOT);(BOT,BOT);(BOT,BOT);(BOT,BOT)|];
  (*  L0 *)   [|(BOT,BOT);  (L0,L0);  (L0,E0);  (L0,G0); (L0,LE0);  (L0,N0); (L0,GE0); (L0,TOP)|];
  (*  E0 *)   [|(BOT,BOT);  (E0,L0);(BOT,BOT);(BOT,BOT);  (E0,L0);  (E0,L0);(BOT,BOT);  (E0,L0)|];
  (*  G0 *)   [|(BOT,BOT);  (G0,L0);(BOT,BOT);(BOT,BOT);  (G0,L0);  (G0,L0);(BOT,BOT);  (G0,L0)|];
  (* LE0 *)   [|(BOT,BOT); (LE0,L0);  (L0,E0);  (L0,G0); (LE0,L0); (LE0,N0); (LE0,G0); (LE0,L0)|];
  (*  N0 *)   [|(BOT,BOT);  (N0,L0);  (L0,E0);  (L0,G0); (N0,LE0);  (N0,N0); (L0,GE0); (N0,TOP)|];
  (* GE0 *)   [|(BOT,BOT); (GE0,L0);(BOT,BOT);(BOT,BOT);  (G0,L0); (GE0,L0);(BOT,BOT); (GE0,L0)|];
  (* TOP *)   [|(BOT,BOT); (TOP,L0);  (L0,E0);  (L0,G0); (L0,LE0); (TOP,N0); (L0,GE0);(TOP,TOP)|]|]

let e0_p_table =
  (* table for chi1 + chi1 = L0 : [= *)
  (*                   BOT        L0        E0        G0       LE0        N0       GE0      TOP      *)
  (* BOT *) [|[|(BOT,BOT);(BOT,BOT);(BOT,BOT);(BOT,BOT);(BOT,BOT);(BOT,BOT);(BOT,BOT);(BOT,BOT)|];
  (*  L0 *)   [|(BOT,BOT);(BOT,BOT);(BOT,BOT);  (L0,G0);(BOT,BOT);  (L0,G0);  (L0,G0);  (L0,G0)|];
  (*  E0 *)   [|(BOT,BOT);(BOT,BOT);  (E0,E0);(BOT,BOT);  (E0,E0);(BOT,BOT);  (E0,E0);  (E0,E0)|];
  (*  G0 *)   [|(BOT,BOT);  (G0,L0);(BOT,BOT);(BOT,BOT);  (G0,L0);  (G0,L0);(BOT,BOT);  (G0,L0)|];
  (* LE0 *)   [|(BOT,BOT);(BOT,BOT);  (E0,E0);  (L0,G0);  (E0,E0);  (L0,G0);(LE0,GE0);(LE0,GE0)|];
  (*  N0 *)   [|(BOT,BOT);  (G0,L0);(BOT,BOT);  (L0,G0);  (G0,L0);  (N0,N0);  (L0,G0);  (N0,N0)|];
  (* GE0 *)   [|(BOT,BOT);  (L0,G0);  (E0,E0);(BOT,BOT);(GE0,LE0);  (L0,G0);  (E0,E0); (GE0,L0)|];
  (* TOP *)   [|(BOT,BOT);  (L0,G0);  (E0,E0);  (G0,L0);(LE0,GE0);  (N0,N0); (GE0,L0);(TOP,TOP)|]|]

let g0_p_table =
  (* table for chi1 + chi1 = G0 : [= *)
  (*                   BOT        L0        E0        G0       LE0        N0       GE0      TOP      *)
  (* BOT *) [|[|(BOT,BOT);(BOT,BOT);(BOT,BOT);(BOT,BOT);(BOT,BOT);(BOT,BOT);(BOT,BOT);(BOT,BOT)|];
  (*  L0 *)   [|(BOT,BOT);(BOT,BOT);(BOT,BOT);  (L0,G0);(BOT,BOT);  (L0,G0);  (L0,G0);  (L0,G0)|];
  (*  E0 *)   [|(BOT,BOT);(BOT,BOT);(BOT,BOT);  (G0,G0);(BOT,BOT);  (E0,G0);  (E0,G0);  (E0,G0)|];
  (*  G0 *)   [|(BOT,BOT);  (G0,L0);  (G0,E0);  (G0,G0); (G0,LE0);  (G0,N0); (G0,GE0); (G0,TOP)|];
  (* LE0 *)   [|(BOT,BOT);(BOT,BOT);(BOT,BOT); (LE0,G0);(BOT,BOT); (LE0,G0); (LE0,G0); (LE0,G0)|];
  (*  N0 *)   [|(BOT,BOT);  (G0,L0);  (G0,E0);  (N0,G0); (G0,LE0);  (N0,N0);  (G0,L0); (N0,TOP)|];
  (* GE0 *)   [|(BOT,BOT);  (G0,L0);  (G0,E0); (GE0,G0); (G0,LE0);  (L0,G0); (GE0,G0); (GE0,N0)|];
  (* TOP *)   [|(BOT,BOT);  (G0,L0);  (G0,E0); (TOP,G0); (G0,LE0); (TOP,N0); (N0,GE0);(TOP,TOP)|]|]

let back_plus chi1 chi2 chi = 
  match chi with
  | BOT -> BOT, BOT
  | L0  -> choose l0_p_table chi1 chi2
  | E0  -> choose e0_p_table chi1 chi2
  | G0  -> choose g0_p_table chi1 chi2
  | LE0 -> 
      let l1, l2 = choose l0_p_table chi1 chi2 in
      let e1, e2 = choose e0_p_table chi1 chi2 in
      (sjoin l1 e1), (sjoin l2 e2)
  | N0  ->
      let l1, l2 = choose l0_p_table chi1 chi2 in
      let g1, g2 = choose g0_p_table chi1 chi2 in
      (sjoin l1 g1), (sjoin l2 g2)
  | GE0 ->
      let g1, g2 = choose g0_p_table chi1 chi2 in
      let e1, e2 = choose e0_p_table chi1 chi2 in
      (sjoin g1 e1), (sjoin g2 e2)
  | TOP -> (smeet chi1 TOP), (smeet chi2 TOP)

let back_minus chi1 chi2 chi =
  match chi with
  | BOT -> BOT, BOT
  | L0  -> neg chi2 |> choose l0_p_table chi1
  | E0  -> neg chi2 |> choose e0_p_table chi1
  | G0  -> neg chi2 |> choose g0_p_table chi1
  | LE0 ->
      let chi2 = neg chi2 in 
      let l1, l2 = choose l0_p_table chi1 chi2 in
      let e1, e2 = choose e0_p_table chi1 chi2 in
      (sjoin l1 e1), (sjoin l2 e2) 
  | N0  ->
      let chi2 = neg chi2 in
      let l1, l2 = choose l0_p_table chi1 chi2 in
      let g1, g2 = choose g0_p_table chi1 chi2 in
      (sjoin l1 g1), (sjoin l2 g2)
  | GE0 ->
      let chi2 = neg chi2 in
      let g1, g2 = choose g0_p_table chi1 chi2 in
      let e1, e2 = choose e0_p_table chi1 chi2 in
      (sjoin g1 e1), (sjoin g2 e2)
  | TOP -> chi1, chi2

let rec back_evala a r chi =
  match a with
  | Num (n) ->
      let sign = 
        if n < 0 then L0
        else if n > 0 then G0
        else E0
      in
      if smeet sign chi <> BOT then r
      else bot () 
  | Var (x) ->
      if (x="x") || (x="y") || (x="z")
      then 
        if (smeet (r x) chi) <> BOT then 
            function y -> if (x=y) then (smeet (r x) chi) else (r y)
        (*then assign x (smeet (r x) chi) r*)
        else bot ()
      else failwith "AbstractDomainSigns : undeclared variable"
  | Plus (a1, a2) ->
      let chi1, chi2 = back_plus (evala a1 r) (evala a2 r) chi in 
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)
  | Minus (a1, a2) ->
      let chi1, chi2 = back_minus (evala a1 r) (evala a2 r) chi in 
      meet (back_evala a1 r chi1) (back_evala a2 r chi2)

let back_lt_table =
  (* table for less than : < *)
  (*                    BOT         L0         E0         G0        LE0         N0        GE0       TOP      *)
  (* BOT *) [|[| (BOT,BOT); (BOT,BOT); (BOT,BOT); (BOT,BOT); (BOT,BOT); (BOT,BOT); (BOT,BOT); (BOT,BOT) |];
  (*  L0 *)   [| (BOT,BOT);   (L0,L0);   (L0,E0);   (L0,G0);  (L0,LE0);   (L0,N0);  (L0,GE0);  (L0,TOP) |];
  (*  E0 *)   [| (BOT,BOT); (BOT,BOT); (BOT,BOT);   (E0,G0); (BOT,BOT);   (E0,G0);   (E0,G0);   (E0,G0) |];
  (*  G0 *)   [| (BOT,BOT); (BOT,BOT); (BOT,BOT);   (G0,G0); (BOT,BOT);   (G0,G0);   (G0,G0);   (G0,G0) |];
  (* LE0 *)   [| (BOT,BOT);   (L0,L0);   (L0,E0);  (LE0,G0);  (L0,LE0);  (LE0,N0); (LE0,GE0); (LE0,TOP) |];
  (*  N0 *)   [| (BOT,BOT);   (L0,L0);   (L0,E0);   (N0,G0);  (L0,LE0);   (N0,N0);  (N0,GE0);  (N0,TOP) |];
  (* GE0 *)   [| (BOT,BOT); (BOT,BOT); (BOT,BOT);  (GE0,G0); (BOT,BOT);  (GE0,G0); (GE0,GE0); (GE0,GE0) |];
  (* TOP *)   [| (BOT,BOT);  (L0,L0);   (L0,E0);   (TOP,G0);  (L0,LE0);  (TOP,N0); (TOP,GE0); (TOP,TOP) |]|]

let back_lt chi1 chi2 = choose back_lt_table chi1 chi2

let back_eq chi1 chi2 = let chi = smeet chi1 chi2 in chi, chi

let back_gt_table =
  (* table for greather than : > *)
  (*                    BOT         L0         E0         G0        LE0         N0        GE0       TOP      *)
  (* BOT *) [|[| (BOT,BOT); (BOT,BOT); (BOT,BOT); (BOT,BOT); (BOT,BOT); (BOT,BOT); (BOT,BOT); (BOT,BOT) |];
  (*  L0 *)   [| (BOT,BOT);   (L0,L0); (BOT,BOT); (BOT,BOT);   (L0,L0);   (L0,L0); (BOT,BOT);   (L0,L0) |];
  (*  E0 *)   [| (BOT,BOT);   (E0,L0); (BOT,BOT); (BOT,BOT); (BOT,BOT);   (E0,L0); (BOT,BOT);   (E0,L0) |];
  (*  G0 *)   [| (BOT,BOT);   (G0,L0);   (G0,E0);   (G0,G0);  (G0,LE0);   (G0,N0);  (G0,GE0);  (G0,TOP) |];
  (* LE0 *)   [| (BOT,BOT);  (LE0,L0); (BOT,BOT); (BOT,BOT);  (LE0,L0);  (LE0,L0); (BOT,BOT);  (LE0,L0) |];
  (*  N0 *)   [| (BOT,BOT);   (N0,L0);   (G0,E0);   (G0,G0);  (N0,LE0);   (N0,N0);  (G0,GE0);  (N0,TOP) |];
  (* GE0 *)   [| (BOT,BOT);  (GE0,L0);   (G0,E0);   (G0,G0); (GE0,LE0);  (GE0,N0);  (G0,GE0); (GE0,TOP) |];
  (* TOP *)   [| (BOT,BOT);  (TOP,L0);   (G0,E0);   (G0,G0); (TOP,LE0);  (TOP,N0);  (G0,GE0); (TOP,TOP) |]|]

let back_gt chi1 chi2 = choose back_gt_table chi1 chi2

let back_neq_table =
  (* table for greather than : != *)
  (*                    BOT         L0         E0         G0        LE0         N0        GE0       TOP      *)
  (* BOT *) [|[| (BOT,BOT); (BOT,BOT); (BOT,BOT); (BOT,BOT); (BOT,BOT); (BOT,BOT); (BOT,BOT); (BOT,BOT) |];
  (*  L0 *)   [| (BOT,BOT);   (L0,L0);   (L0,E0);   (L0,G0);  (L0,LE0);   (L0,N0);  (L0,GE0);  (L0,TOP) |];
  (*  E0 *)   [| (BOT,BOT);   (E0,L0); (BOT,BOT);   (E0,G0);   (E0,L0);   (E0,N0);   (E0,G0);   (E0,N0) |];
  (*  G0 *)   [| (BOT,BOT);   (G0,L0);   (G0,E0);   (G0,G0);  (G0,LE0);   (G0,N0);  (G0,GE0);  (G0,TOP) |];
  (* LE0 *)   [| (BOT,BOT);  (LE0,L0);   (L0,E0);  (LE0,G0);  (LE0,L0);  (LE0,N0);  (LE0,G0);  (LE0,N0) |];
  (*  N0 *)   [| (BOT,BOT);   (N0,L0);   (N0,E0);   (N0,G0);  (N0,LE0);   (N0,N0);  (N0,GE0);  (N0,TOP) |];
  (* GE0 *)   [| (BOT,BOT);  (GE0,L0);   (G0,E0);  (GE0,G0);  (GE0,L0);  (GE0,N0);  (GE0,G0);  (GE0,N0) |];
  (* TOP *)   [| (BOT,BOT);  (TOP,L0);   (N0,E0);  (TOP,G0);  (N0,LE0);  (TOP,N0);  (N0,GE0); (TOP,TOP) |]|]

let back_neq chi1 chi2 = choose back_neq_table chi1 chi2

let back_gte chi1 chi2 =
  let lt1, lt2 = back_gt chi1 chi2 in 
  let e1, e2 = back_eq chi1 chi2 in
  (sjoin lt1 e1), (sjoin lt2 e2)

let back_lte chi1 chi2 =
  let gt1, gt2 = back_gt chi1 chi2 in 
  let e1, e2 = back_eq chi1 chi2 in
  (sjoin gt1 e1), (sjoin gt2 e2)

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

let widen_sign s1 s2 = s2

let widen r1 r2 = function x -> widen_sign (r1 x) (r2 x)

let narrow_sign s1 s2 = s2

let narrow r1 r2 = function x -> narrow_sign (r1 x) (r2 x)