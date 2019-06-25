(* file fixpoint.ml *)
(* implemented by Goktug Saatioclu *)

(* Old fixpoint without widening and narrowing *)
 
let rec lfp_old r leq f =
  let r' = f r in
  if leq r' r then r'
  else lfp_old r' leq f


(* successor widening *)
let rec successor_widening r leq f w =
  let r' = f r in
  if leq r' r then r
  else successor_widening (w r r') leq f w

(* successor narrowing *)
let rec successor_narrowing r leq f n =
  let r' = f r in
  let r'' = n r r' in
  if leq r r'' then r'
  else successor_narrowing r'' leq f n

(* find a fixpoint via widening and narrowing *)
let lfp r leq f w n =
  (* let r = f r in *) (* is this line necessary? *)
  let widened = successor_widening r leq f w in 
  let res = successor_narrowing widened leq f n in 
  res