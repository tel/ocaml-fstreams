
(** Total lazy streams.

    Streams are lazy data structures which contain values in
    sequence. Due to laziness, streams may be infinite (e.g. {!ints}
    below). The streams in this module are "total" referring to the
    fact that eliminators like {!head}, {!tail}, and {!uncons} return
    pure values implying that there is never an end to the sequence of
    contained values.

    The streams in {!Partial} are a sister datatype to these streams
    which is "partial", e.g. {!Partial.uncons} returns values wrapped
    in [option].

    Mathematically, total streams are the greatest fixed point of the
    functor [F X = A * X].

*)

(** A total lazy stream; a necessarily unbounded sequence of values of
    type ['a]. *)
type +'a t

val head : 'a t -> 'a
val tail : 'a t -> 'a t
val uncons : 'a t -> ('a * 'a t)
val cons : 'a -> 'a t -> 'a t

(** Interleaves two streams *)
val alt : 'a t -> 'a t -> 'a t
val fold : ('a -> 'r Lazy.t -> 'r) -> ('a t -> 'r)
val unfold : ('s -> 'a * 's) -> ('s -> 'a t)
val trajectory : ('a -> 'a) -> ('a -> 'a t)
val impure : (unit -> 'a) -> 'a t

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
