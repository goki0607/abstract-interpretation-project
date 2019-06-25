(* file abstractDomain.mli  © P. Cousot 2018 *)

open AbstractSyntaxExpressions

type t
val initialize : variable list -> unit
val leq : t -> t -> bool
val bot : unit -> t
val join :  t -> t -> t
val initialP : unit -> t
val assign : variable -> aexpr -> t -> t
val test : bexpr -> t -> t
val nottest : bexpr -> t -> t
val stringofaP : t -> string
val widen : t -> t -> t
val narrow : t -> t -> t
