(** Total, lazy, functional streams; guaranteed to be unending.

    Streams are lazy data structures which contain values in
    sequence. Due to laziness, streams may be infinite (e.g. {!ints}
    below). The streams in this module are "total" referring to the
    fact that {{!elimination}[eliminators]} like {!head}, {!tail}, and
    {!uncons} return pure values implying that there is never an end
    to the sequence of contained values.

    The streams in {!Partial} are a sister datatype to these streams
    which is "partial", e.g. {!Partial.uncons} returns values wrapped
    in [option].

    Mathematically, total streams are the greatest fixed point of the
    functor [F X = A * X].

*)

(** A total lazy stream; a necessarily unbounded sequence of values of
    type ['a]. *)
type +'a t



(** {1:introduction Value introduction } *)

val cons : 'a -> 'a t -> 'a t
(** Extend a stream by prepending a value. *)

val unfold : ('s -> 'a * 's) -> ('s -> 'a t)
(** Lazily unfolds a total stream. In each step of [unfold build s],
    the current seed is passed to [build] the next value of the stream
    and the new seed value, e.g.

    {[ let ints : int t = unfold (fun n -> (n, n+1)) 0 ]}
*)
                                
val trajectory : ('a -> 'a) -> ('a -> 'a t)
(** Generate an infinite stream from the trajectory of a
    endomorphism. In other words, [head (trajectory f a) = a] and
    [tail (trajectory f a) = trajectory f (f a)] *)

val impure : (unit -> 'a) -> 'a t
(** Generates a list impurely. Each new value of the stream [impure f]
    is produced by calling [f ()]. See also {!tabulate}. *)

val tabulate : (int -> 'a) -> 'a t
(** Generates a stream by tabulation of values.

    Total streams can be seen as memoizations of functions of type
    [int -> 'a], so {!tabulate} witnesses part of this isomorphism.

    See {!nth}.
*)
    
val ints : int t
(** An infinite stream of all integers. *)

(** {i See also}: {!pure} *)




(** {1:elimination Value elimination } *)

val head : 'a t -> 'a
val tail : 'a t -> 'a t
    
val uncons : 'a t -> ('a * 'a t)
(** The "principle eliminator" for a stream. Can be used along with
    recursion to derive all of the others. *)

val fold : ('a -> 'r Lazy.t -> 'r) -> ('a t -> 'r)
(** Non-strict right fold. One must be careful in [f] about forcing
    the second argument only as needed; [fold f z s] {i can} return in
    finite time, but won't if it is too strict.

    This is the principle recursor of a total stream. Essentially any
    function eliminating streams can be derived from {!fold}.
*)

val iter : ('a -> unit) -> ('a t -> Void.t)
(** Impure consumption of a stream. {i Note} that this function will
    never return. *)

val nth : int -> 'a t -> 'a
(** Convert a stream into an accessor function on index. See
    {!tabulate}. *)



(** {1:functor Streams are {i covariant functors}} *)

val map : ('a -> 'b) -> ('a t -> 'b t)
(** Applies a function valuewise to a stream. *)


(** {1:applicative Streams are {i applicative} functors} *)
(** Streams are "zippy" applicative functors. *)

val pure : 'a -> 'a t
(** Produces the infinite, constant stream of some value. *)

val ap : ('a -> 'b) t -> ('a t -> 'b t)
(** "Zips" a stream of functions with a stream of their arguments
    returning the resulting list. *)

val map2 : ('a -> 'b -> 'c) -> ('a t -> 'b t -> 'c t)
(** A generalization of {!map} afforded by {!pure} and {!ap}. This
    version may be more efficient than the equivalent version produced
    using {!pure} and {!ap}.
    {[ map2 f a b = ap (ap (pure f) a) b ]}
*)

val map3 : ('a -> 'b -> 'c -> 'd ) -> ('a t -> 'b t -> 'c t -> 'd t)
(** A generalization of {!map} afforded by {!pure} and {!ap}. This
    version may be more efficient than the equivalent version produced
    using {!pure} and {!ap}.
    {[ map3 f a b c = ap (ap (ap (pure f) a) b) c ]}
*)


(** {1:applicative Streams are {i comonads}} *)
(** Streams are comonads focused on their "zero" element. *)

val extract : 'a t -> 'a
(** Extract the focused element, e.g. [extract s = nth s 0].*)

val extend : ('a t -> 'b) -> ('a t -> 'b t)
(** Apply a function from streams to summary values at all points in
    time. In other words [head (extend f s) = f s] and [tail (extend f
    s) = extend f (push s)]. *)



(** {1:extras Other operations } *)


(** Interleaves two streams *)
val interleave : 'a t -> 'a t -> 'a t
(** Interleaves two streams, non-associative. For instance, the
    streams [let x = [1;2;3;...]] and [let y = [a;b;c;...]] are
    interwoven to form [interleave x y = [1;a;2;b;3;c;...]]. *)

val push : 'a t -> 'a t
(** If a stream [s] is interpreted as a process through time then
    [delay s] is the same process beginning at {v t=-1 v} instead of
    {v t=0 v}.

    For total streams, {!delay} is identical to {!tail}.
*)

val take : int -> 'a t -> 'a list
(** Convert a prefix of a stream into a [list]. Note that the list
    [take n s] is not necessarily as long as [n]. *)

val drop       : int -> 'a t -> 'a t
(** Trim off a prefix of a stream. *)
    
val inits      : 'a t -> 'a list t
(** Stream all prefixes of a stream. *)

val tails      : 'a t -> 'a t t
(** Stream all suffixes of a stream. *)
    
val keep       : ('a -> 'b option) -> ('a t -> 'b t)
(** Transform a stream dropping some elements. See {!map}. *)

val filter     : ('a -> bool) -> ('a t -> 'a t)
(** Dropping some elements of a stream. See {!keep}. *)


(** {1:impl Implementation} *)

(** The stream implementation is exposed to allow certain tricky
    definitions. {i This will hopefully be removed in later
    versions, try not to depend upon it!} *)

module Impl : sig
  type 'a t = 'a q Lazy.t
  and 'a q = { head : 'a; tail : 'a t }
end
