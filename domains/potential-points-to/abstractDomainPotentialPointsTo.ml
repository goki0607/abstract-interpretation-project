(** file abstractDomainPotentialPointsTo.ml
  * implemented by Goktug Saatcioglu *)

open AbstractSyntaxExpressions

module OrderedString =
  struct
    type t = string
    let compare = Pervasives.compare
  end

module StringSet = Set.Make(OrderedString)

type abstractLocation = 
  | BOT
  | NIL
  | Loc of StringSet.t
  | TOP

exception ERR of string

let lleq l1 l2 =
  match l1, l2 with
  | BOT, _ -> true
  | _, BOT -> false
  | _, TOP -> true
  | TOP, _ -> false
  | NIL, _ -> true
  | _, NIL -> false
  | Loc s1, Loc s2 -> StringSet.subset s1 s2
    
let ljoin l1 l2 =
  match l1, l2 with
  | BOT, _ -> l2
  | _, BOT -> l1
  | TOP, _ -> TOP
  | _, TOP -> TOP
  | NIL, _ -> l2
  | _, NIL -> l1
  | Loc s1, Loc s2 -> Loc (StringSet.union s1 s2)

let lmeet l1 l2 =
  match l1, l2 with
  | BOT, _ -> BOT
  | _, BOT -> BOT
  | TOP, _ -> l2
  | _, TOP -> l1
  | NIL, _ -> NIL
  | _, NIL -> NIL
  | Loc s1, Loc s2 -> Loc (StringSet.inter s1 s2)

let stringofloc l =
  match l with
  | BOT -> "_|_"
  | NIL -> "null"
  | Loc s ->
      let str = StringSet.fold 
        (fun x acc -> if acc="" then "\\"^x else acc^", \\"^x)
        s
        ""
      in 
      "["^str^"]"
  | TOP -> "T"

(* environments are represented as a function of "x", "y" and "z" only *)
type t = string -> abstractLocation

let leq r1 r2 = (lleq (r1 "x") (r2 "x")) &&
                (lleq (r1 "y") (r2 "y")) &&
                (lleq (r1 "z") (r2 "z"))

let initialize vl = ()

let bot () = function x -> BOT

let meet r1 r2 = function x -> lmeet (r1 x) (r2 x)

let join r1 r2 = function x -> ljoin (r1 x) (r2 x)

let initialP () = function x -> NIL

let rec evala a r =
  match a with
  | Num n -> BOT
  | Var x ->
      if (x="x") then (r x)
      else if (x="y") then (r x)
      else if (x="z") then (r x)
      else failwith "AbstractDomainPointsTo : undeclared variable"
  | Null -> BOT
  | Addr x -> 
      if (x="x") then Loc (StringSet.(empty |> add "x"))
      else if (x="y") then Loc (StringSet.(empty |> add "y"))
      else if (x="z") then Loc (StringSet.(empty |> add "z"))
      else failwith "AbstractDomainPointsTo : undeclared variable"
  | Deref p ->
      if (p="x") || (p="y") || (p="z") then
        match (r p) with
        | BOT -> BOT
        | NIL -> raise (ERR ("Error: Cannot dereference null pointer."))
        | Loc l -> StringSet.(fold (fun x acc -> ljoin (r x) acc) l BOT)
        | TOP -> TOP
      else failwith "AbstractDomainPointsTo : undeclared variable"
  | Plus (a1, a2) -> BOT
  | Minus (a1, a2) -> BOT

let collect p r =
  match (r p) with
  | BOT -> BOT
  | NIL -> raise (ERR ("Error: Cannot dereference null pointer."))
  | Loc l -> StringSet.(fold (fun x acc -> ljoin (r x) acc) l BOT)
  | TOP -> TOP
  
let assign x a r = function y -> if (r x) = BOT then BOT
                                 else if (x=y) then (evala a r)
                                 else (r y)

let assignP x a r = function y ->
                      if (r x) = BOT then BOT
                      else if (lleq (collect x r) (r x)) then ljoin (r x) (evala a r)
                      else if ((lleq (collect x r) (r x)) |> not) then ljoin (r x) BOT
                      else (r y)
                        

let smash p1 p2 r = (match (p1,p2) with
   | BOT, _ -> bot ()
   | _, BOT -> bot ()
   | _, _ -> r)

let stringofaP r = "x=" ^ (stringofloc (r "x")) ^ ", y=" ^ (stringofloc (r "y")) ^ ", z=" ^ (stringofloc (r "z"))

let rec back_evala a r chi =
  match a with
  | Num n -> bot ()
  | Var x ->
      if (x="x") || (x="y") || (x="z")
      then 
        if (lmeet (r x) chi) <> BOT then 
            function y -> if (x=y) then (lmeet (r x) chi) else (r y)
        (*then assign x (smeet (r x) chi) r*)
        else bot ()
      else failwith "AbstractDomainSigns : undeclared variable"
  | Null -> if lmeet NIL chi <> BOT then r else bot ()
  | Addr x -> 
      let l' =
        if (x="x") then Loc (StringSet.(empty |> add "x"))
        else if (x="y") then Loc (StringSet.(empty |> add "y"))
        else if (x="z") then Loc (StringSet.(empty |> add "z"))
        else failwith "AbstractDomainPointsTo : undeclared variable"
      in 
      if lmeet l' chi <> BOT then
        function y -> if (x=y) then (lmeet l' chi) else (r y)
      else bot ()
  | Deref p ->
      let l' = 
        if (p="x") || (p="y") || (p="z") then
          match (r p) with
          | BOT -> BOT
          | NIL -> raise (ERR ("Error: Cannot dereference null pointer."))
          | Loc l -> StringSet.(fold (fun x acc -> ljoin (r x) acc) l BOT)
          | TOP -> TOP
        else failwith "AbstractDomainPointsTo : undeclared variable"
      in 
      if lmeet l' chi <> BOT then r else bot ()
  | Plus (a1, a2) -> bot ()
  | Minus (a1, a2) -> bot ()

let rec back_test b r =
  match b with 
  | Lt (a1, a2) -> bot ()
  | Eq (Var (v1), Var (v2)) ->
      (let a = lmeet (r v1) (r v2) in 
      if a = BOT then bot ()
      else
        function y -> if (y=v1) || (v2=y) then a else (r y))
  | Eq (Var (v), Null) ->
      (let a = lmeet (r v) NIL in 
      if a <> NIL then bot ()
      else
        function y -> if (v=y) then NIL else (r y))
  | Eq (a1, a2) -> bot ()
  | Neq (a1, a2) -> bot ()
  | Gt (a1, a2) -> bot ()
  | Nand (b1, b2) ->
      let a = meet (back_nottest b1 r) (back_nottest b2 r) in
      let b = meet (back_nottest b1 r) (back_test b2 r) in
      let c = meet (back_test b1 r) (back_nottest b2 r) in
      join a b |> join c
and
back_nottest b r =
  match b with 
  | Lt (a1, a2) -> bot ()
  | Eq (Var (v1), Var (v2)) ->
      (if (lleq (r v1) NIL) && (lleq (r v2) NIL) then bot ()
      else r)
  | Eq (Var (v), Null) ->
      if lleq (r v) NIL then bot () else r
  | Eq (a1, a2) -> bot ()
  | Neq (a1, a2) -> bot ()
  | Gt (a1, a2) -> bot ()
  | Nand (b1, b2) ->
      meet (back_test b1 r) (back_test b2 r)
      
let test b r = back_test b r

let nottest b r = back_nottest b r

let widen_l l1 l2 = l2

let widen r1 r2 = function x -> widen_l (r1 x) (r2 x)

let narrow_l l1 l2 = l2

let narrow r1 r2 = function x -> narrow_l (r1 x) (r2 x)