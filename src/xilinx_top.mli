open Base

val generate
  :  ?custom_constraints:Rope.t
  -> name:string
  -> part:string
  -> pins:Pin.t list
  -> Board.t
  -> unit
