
type 'a t = 'a option

val fold : ('a -> 'r) -> 'r -> ('a t -> 'r)
val map  : ('a -> 'b) -> ('a t -> 'b t)
val pure : 'a -> 'a t
val ap   : ('a -> 'b) t -> ('a t -> 'b t)
val zero : 'a t
val bind : ('a -> 'b t) -> ('a t -> 'b t)

module First : sig
  val prod : 'a t -> 'a t -> 'a t
end

module Last : sig
  val prod : 'a t -> 'a t -> 'a t
end
