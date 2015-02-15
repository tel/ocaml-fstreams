(** Infinite streams *)
type +'a t

val head : 'a t -> 'a
val tail : 'a t -> 'a t
val uncons : 'a t -> ('a * 'a t)
val cons : 'a -> 'a t -> 'a t

(** Interleaves two streams *)
val alt : 'a t -> 'a t -> 'a t
val fold : ('a -> 'r Lazy.t -> 'r) -> ('a t -> 'r Lazy.t)
val unfold : ('s -> 'a * 's) -> ('s -> 'a t)
val trajectory : ('a -> 'a) -> ('a -> 'a t)
val generate : (unit -> 'a) -> 'a t

(** Iterates down values of the stream forever. *)
val iter : ('a -> unit) -> ('a t -> unit)
val map : ('a -> 'b) -> ('a t -> 'b t)
val ints : int t
val tabulate : (int -> 'a) -> 'a t
val pure : 'a -> 'a t
val ap : ('a -> 'b) t -> ('a t -> 'b t)
val take : int -> 'a t -> 'a list
val drop : int -> 'a t -> 'a t
val inits : 'a t -> 'a list t
val tails : 'a t -> 'a t t

val nth : int -> 'a t -> 'a
val keep : ('a -> 'b option) -> ('a t -> 'b t)
val filter : ('a -> bool) -> ('a t -> 'a t)

val extract : 'a t -> 'a
val extend  : ('a t -> 'b) -> ('a t -> 'b t)
