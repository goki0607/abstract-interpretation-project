(* file abstractSyntaxExpressions.ml  © P. Cousot 2018 *)

type variable = string
type aexpr = Num of int | Var of string | Minus of aexpr * aexpr | Plus of aexpr * aexpr
type bexpr = Lt of aexpr * aexpr | Gt of aexpr * aexpr | Eq of aexpr * aexpr | Neq of aexpr * aexpr | Nand of bexpr * bexpr

let varsa a =
   let rec collecta a = match a with 
      | Num i -> [] 
      | Var x -> [x]
      | Minus (a1,a2) -> (collecta a1)@(collecta a2)
      | Plus (a1,a2) -> (collecta a1)@(collecta a2)
   and cmp x y = if x=y then 0
                 else if x<y then 1
                 else -1
   in List.sort_uniq cmp (collecta a)

let varsb b =
   let rec collectb b = match b with 
      | Lt (a1,a2) -> (varsa a1)@(varsa a2)
      | Eq (a1,a2) -> (varsa a1)@(varsa a2)
      | Neq (a1,a2) -> (varsa a1)@(varsa a2)
      | Gt (a1,a2) -> (varsa a1)@(varsa a2)
      | Nand (b1,b2) -> (collectb b1)@(collectb b2)   
   and cmp x y = if x=y then 0
                 else if x<y then 1
                 else -1
   in List.sort_uniq cmp (collectb b)
