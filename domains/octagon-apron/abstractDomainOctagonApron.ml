(** file abstractDomainOctagonApron.ml
  * implemented by Goktug Saatcioglu *)

open AbstractSyntaxExpressions
open Apron

(* the octagon manager *)
let manoct = Oct.manager_alloc ()

(* only three variables *)
let var_x = Var.of_string "x"
let var_y = Var.of_string "y"
let var_z = Var.of_string "z"

(* an environment to be used throughougt *)
(* we only assume there are three variables *)
let env = Environment.make [|var_x; var_y; var_z|] [||]

type abstractOctagon = Oct.t Abstract1.t

let oleq o1 o2 =
  Abstract1.is_leq manoct o1 o2

let ojoin o1 o2 =
  Abstract1.join manoct o1 o2

let omeet o1 o2 =
  Abstract1.meet manoct o1 o2 

let osum s1 s2 = s1^"+"^s2

let omin s1 s2 = s1^"-"^s2

let oupdate r var texpr =
  if Abstract1.is_bottom manoct r then r
  else Abstract1.assign_texpr manoct r var texpr None

(* this is no longer necessary but keep just in case *)
let stringofoct r var =
  let inter = Abstract1.bound_variable manoct r var in
    (*Interval.print Format.std_formatter inter;*)
    if Interval.is_bottom inter then "_|_"
    else if Interval.is_top inter then "T" 
    else
      let str =
        let ppf = Format.str_formatter in
        Interval.print ppf inter;
        Format.flush_str_formatter ()
      in 
      str

(* environments are represented as a function of "x", "y" and "z" only *)
type t = abstractOctagon

let leq r1 r2 = oleq r1 r2

let initialize vl = ()

let bot () = Abstract1.bottom manoct env

let meet r1 r2 = omeet r1 r2

let join r1 r2 = ojoin r1 r2

(* we implicitly assume all variables are top initialized *)
(* assume all numbers are integers *)
let initialP () = 
  (* below comments is if you want all variables zero initialized *)
  (*
  let tab = Parser.lincons1_of_lstring env ["x=0";"y=0";"z=0"] in 
  Abstract1.of_lincons_array manoct env tab
  *)
  Abstract1.top manoct env

(* this just checks for any incorrect declared variables *)
(* its inefficient but I don't want to break the current logic *)
let rec evala' a =
  match a with
  | Num (n) -> string_of_int n
  | Var (x) ->
      if (x="x") || (x="y") || (x="z") then x
      else failwith "AbstractDomainOctagon : undeclared variable"
  | Plus (a1, a2) -> osum (evala' a1) (evala' a2)
  | Minus (a1, a2) -> omin (evala' a1) (evala' a2)

let rec collect_var a t =
  match a with
  | Num (n) -> 0
  | Var (x) -> if x=t then 1 else 0
  | Plus (a1, a2) -> collect_var a1 t + collect_var a2 t
  | Minus (a1, a2) -> collect_var a1 t - collect_var a2 t

(* this collects all the numbers in an expression *)
let rec simplify_num a =
  match a with
  | Num (a) -> a
  | Var (x) -> 0
  | Plus (a1, a2) -> simplify_num a1 + simplify_num a2
  | Minus (a1, a2) -> simplify_num a1 - simplify_num a2

(* this makes the final expression that has been evaluated *)
let make x y z n =
  let xs = [x; y; z; n] in 
  let rec traverse acc xs =
    match xs with
    | [] -> acc
    | hd :: tl when hd <> "" -> traverse (hd :: acc) tl
    | _ :: tl -> traverse acc tl
  in 
  let xs = traverse [] xs in 
  let res =
    if xs = [] then failwith "AbstractDomainOctagon : problem simplifying expression"
    else
      let rec join xs =
        match xs with
        | [] -> ""
        | [hd] -> hd
        | hd :: tl -> hd^"+"^(join tl)
      in
      join xs
  in 
  res

(* this first collects all same variables and the numbers *)
(* then it simplifies the variables *)
(* then it builds the final expression *)
(** this is all necessary as apron does not accept more than one
  * constant or more than one of the same variable
  * e.g. x+x+5+6 must be simplified to 2x+11
*)
let exp_builder a =
  let xnum = collect_var a "x" in
  let str = if xnum = 0 then "" else (string_of_int xnum)^"*"^"x" in
  let ynum = collect_var a "y" in 
  let str = if ynum = 0 then str 
           else
             if str = "" then (string_of_int ynum)^"*"^"y"
             else str^"+"^(string_of_int ynum)^"*"^"y"
  in 
  let znum = collect_var a "z" in
  let str = if znum = 0 then str 
           else
             if str = "" then (string_of_int znum)^"*"^"z"
             else str^"+"^(string_of_int znum)^"*"^"z"
  in 
  let str' = str in 
  let n = simplify_num a in 
  let str = if str = "" then (string_of_int n)
            else str^"+"^(string_of_int n)
  in 
  str, str', n

let evala a r =
  let _ = evala' a in
  let exp, _, _ = exp_builder a in
  Parser.texpr1_of_string env exp

let assign x a r = 
  let var = if x="x" then var_x
            else if x="y" then var_y
            else if x="z" then var_z
            else failwith "AbstractDomainOctagon : undeclared variable"
  in 
  (oupdate r var (evala a r))

(* no need for smash anymore
let smash p1 p2 r =
  if isbottom p1 || isbottom p2 then bot ()
  else r
*)

(* old implementation, below one is better *)
(*
let stringofaP r =
  "x=" ^ (stringofoct r var_x) ^ ", y=" ^ (stringofoct r var_y) ^ ", z=" ^ (stringofoct r var_z)
*)

let stringofaP r =
  let str =
    (* is such a minimization necessary? *)
    (* Abstract1.minimize_environment manoct r; *)
    let ppf = Format.str_formatter in
    Abstract1.print ppf r;
    Format.flush_str_formatter ()
  in 
  str

let test_simplifier a op op_str =
  let exp, exp', num = exp_builder a in 
  let ans =
    if exp' <> "" then
          [exp'^op_str^(string_of_int (-1*num))]
    else
      if (op num 0) then ["0 = 0"]
      else ["0 < 0"]
  in 
  ans

let parse = Parser.tcons1_of_lstring env

let meet_arr = Abstract1.meet_tcons_array manoct

(* this also needs simplication which is done below *)
let rec test b r =
  match b with
  | Lt (a1, a2) -> 
      let t = test_simplifier (Minus (a1, a2)) ( < ) ("<") in 
      parse t |> meet_arr r
  | Eq (a1, a2) -> 
      let t = test_simplifier (Minus (a1, a2)) ( = ) ("=") in 
      parse t |> meet_arr r
  | Neq (a1, a2) -> 
      let t1 = test_simplifier (Minus (a1, a2)) ( < ) ("<") in 
      let t2 = test_simplifier (Minus (a1, a2)) ( > ) (">") in 
      let t1' = parse t1 |> meet_arr r in 
      let t2' = parse t2 |> meet_arr r in 
      join t1' t2'
  | Gt (a1, a2) -> 
      let t = test_simplifier (Minus (a1, a2)) ( > ) (">") in 
      parse t |> meet_arr r
  | Nand (b1, b2) ->
      let t1, t2 = nottest b1 r, nottest b2 r in
      let t3, t4 = test b1 r, nottest b2 r in 
      let t5, t6 = nottest b1 r, test b2 r in 
      let a = meet t1 t2 in 
      let b = meet t3 t4 in 
      let c = meet t5 t6 in
      join a b |> join c
and
nottest b r =
  match b with
  | Lt (a1, a2) ->
      let t = test_simplifier (Minus (a1, a2)) ( >= ) (">=") in
      parse t |> meet_arr r
  | Eq (a1, a2) ->
      let t1 = test_simplifier (Minus (a1, a2)) ( < ) ("<") in 
      let t2 = test_simplifier (Minus (a1, a2)) ( > ) (">") in 
      let t1' = parse t1 |> meet_arr r in 
      let t2' = parse t2 |> meet_arr r in 
      join t1' t2'
  | Neq (a1, a2) ->
      let t = test_simplifier (Minus (a1, a2)) ( = ) ("=") in
      parse t |> meet_arr r
  | Gt (a1, a2) -> 
      let t = test_simplifier (Minus (a1, a2)) ( <= ) ("<=") in 
      parse t |> meet_arr r
  | Nand (b1, b2) ->
      let t1, t2 = test b1 r, test b2 r in
      meet t1 t2

let widen r1 r2 = Abstract1.widening manoct r1 r2

let narrow r1 r2 = r2