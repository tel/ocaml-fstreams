(** Partial, lazy, functional streams; may terminate or may not.

    Streams are lazy data structures which contain values in
    sequence. Due to laziness, streams may be infinite (e.g. {!ints}
    below). The streams in this module are "partial" referring to the
    fact that {{!elimination}[eliminators]} like {!head},
    {!tail}, and {!uncons} return values wrapped in [option] producing
    [None] when if the stream contains no further values.

    The streams in the {!Total} module are a sister datatype to these
    streams which is "total", e.g. {!Total.uncons} returns pure
    values.

    Mathematically, partial streams are the greatest fixed point of
    the functor [F X = 1 + A * X].

*)

(** A partial lazy stream; a potentially unbounded sequence of values
    of type ['a]. *)
type +'a t



(** {1:introduction Value introduction } *)


val empty : 'a t
(** Partial streams admit an empty value. *)

val cons : 'a -> 'a t -> 'a t
(** Extend a stream by prepending a value. *)

val unfold : ('s -> ('a * 's) option) -> ('s -> 'a t)
(** Lazily unfolds a partial stream. In each step of [unfold build s],
    the current seed is passed to [build] to produce either [None],
    indicating the stream has now terminated, or [Some (a, s')] giving
    the next value of the stream and the new seed value, e.g.

    {[ let ints : int t = unfold (fun n -> Some (n, n+1)) 0 ]}
*)

val trajectory : ('a -> 'a) -> ('a -> 'a t)
(** Generate an infinite stream from the trajectory of a
    endomorphism. In other words, [head (trajectory f a) = a] and
    [tail (trajectory f a) = trajectory f (f a)] *)

val impure : (unit -> 'a option) -> 'a t
(** Generates a list impurely. Each new value of the stream [impure f]
    is produced by calling [f ()] until it is [None]. See also
    {!tabulate}. *)

val of_list : 'a list -> 'a t
(** Lists can be seen as equivalent to necessarily finite
    streams. This function injects a list into the type of partial
    streams.

    In particular, a linked list is the least fixed point of the
    functor [F X = 1 + A * X], the same functor that partial streams
    are the greatest fixed point of.
*)

val tabulate : (int -> 'a option) -> 'a t
(** Generates a stream by tabulation of values. The generation
    proceeds sequentially such that the stream [tabulate f] is either
    infinite or has length equal to the {i first} [n] such that [f n =
    None].

    Partial streams can be seen as memoizations of functions of type
    [int -> 'a option] so long as the "compaction" property above
    holds. This correspondence is nicer for {!Total} streams (see
    {!Total.tabulate}).

    See {!nth}.
*)

val ints : int t
(** An infinite stream of all integers. *)

(** {i See also}: {!pure} *)



(** {1:elimination Value elimination } *)



val head : 'a t -> 'a option
val tail : 'a t -> 'a t option

val uncons : 'a t -> ('a * 'a t) option
(** The "principle eliminator" for a stream. Can be used along with
    recursion to derive all of the others. The fact that the return
    value is optional indicates the potentially finite nature of
    partial streams. *)

val fold : ('a -> 'r Lazy.t -> 'r) -> 'r -> ('a t -> 'r)
(** Non-strict right fold. If [f] is careful about forcing its second
    argument only as needed then [fold f z s] can return in finite
    time even when [s] is infinite.

    This is the principle recursor of a partial stream. Essentially
    any function eliminating streams can be derived from {!fold}.
*)
                                                  
val fold_left : ('r -> 'a -> 'r) -> 'r -> ('a t -> 'r)
(** Strict left fold. If [s] is infinite then [fold_left f z s] will
    never terminate. On the other hand, [fold_left] must be
    tail-recursive. *)

val iter : ('a -> unit) -> ?finally:(unit -> unit) -> ('a t -> unit)
(** Impure consumption of a stream. If the end of the stream is
    reached then the [finally] callback will be invoked. {i This
    function is dangerous.} It may be the case that the consumed
    stream is infinite and therefore [iter f s] will not
    return. Consider calling it asynchronously. *)

val nth : 'a t -> (int -> 'a option)
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
    returning the resulting list. If one stream is shorter than the
    other then the result will be the length of the shorter stream. *)

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
                                        


(** {1:extras Other operations } *)

val interleave : 'a t -> 'a t -> 'a t
(** Interleaves two streams, non-associative. For instance, the
    streams [let x = [1;2;3;...]] and [let y = [a;b;c;...]] are
    interwoven to form [interleave x y = [1;a;2;b;3;c;...]]. *)

val concat : 'a t -> 'a t -> 'a t
(** Concatenates finite streams, one then the next. Streams are
    monoidal under the {!empty} stream and {!concat}. *)

val push : 'a t -> 'a t
(** If a stream [s] is interpreted as a process through time then
    [push s] is the same process beginning at {v t=-1 v} instead of {v
    t=0 v}. *)

val take : int -> 'a t -> 'a list
(** Convert a prefix of a stream into a [list]. Note that the list
    [take n s] is not necessarily as long as [n]. *)

val drop : int -> 'a t -> 'a t
(** Trim off a prefix of a stream. *)
    
val inits : 'a t -> 'a list t
(** Stream all prefixes of a stream. *)

val tails : 'a t -> 'a t t
(** Stream all suffixes of a stream. *)
    
val keep : ('a -> 'b option) -> ('a t -> 'b t)
(** Transform a stream dropping some elements. See {!map}. *)

val filter : ('a -> bool) -> ('a t -> 'a t)
(** Dropping some elements of a stream. See {!keep}. *)


(** {1:impl Implementation} *)

(** The stream implementation is exposed to allow certain tricky
    definitions. {i This will hopefully be removed in later
    versions, try not to depend upon it!} *)

module Impl : sig
  type 'a t = 'a q Lazy.t
  and 'a q = 
    | Cons of 'a * 'a t
    | Empty
end
