module type Folded = sig
  type +'a t
  type +'a thunk

  module Totality : Wrappers.Totality.S

  val embed : ('a * 'a t) Totality.t thunk -> 'a t
  (** The "principle introduction form" for a stream. Can be used along with
      recursion to derive all of the others. The fact that the return
      value is potentially partial indicates the potentially finite nature of
      streams. *)

  val project : 'a t -> ('a * 'a t) Totality.t
  (** The "principle elimination form" for a stream. Can be used along with
      recursion to derive all of the others. The fact that the return
      value is potentially partial indicates the potentially finite nature of
      streams. *)

  val head : 'a t -> 'a Totality.t
  val tail : 'a t -> 'a t Totality.t

  val church : (('a * 'r thunk) Totality.t -> 'r) -> ('a t -> 'r)
  (** The induction form for a stream. Used to derive [fold]s. *)

  val unfold : ('s -> ('a * 's) Totality.t) -> ('s -> 'a t)
  (** Lazily unfolds a partial stream. In each step of [unfold build s],
      the current seed is passed to [build] to produce a new value and a
      new seed. If the generation fails then this signals the end of the
      stream as appropriate.

      {[ let ints : int t = unfold (fun n -> Some (n, n+1)) 0 ]}
  *)

  val iter : ('a -> unit) -> ('a t -> Totality.zero)
  (** Impure consumption of a stream. Note that this function may never
      return. For total streams it certainly won't and this fact is evidenced
      by [Void.t]. *)

  val trajectory : ('a -> 'a) -> ('a -> 'a t)
  (** Generate an infinite stream from the trajectory of a
      endomorphism. In other words, [head (trajectory f a) = a] and
      [tail (trajectory f a) = trajectory f (f a)] *)

  val impure : (unit -> 'a Totality.t) -> 'a t
  (** Generates a list impurely. Each new value of the stream [impure f]
      is produced by calling [f ()]. Failure of [f] corresponds to termination
      of the stream as appropriate. *)
end

module type Fixed = sig
  type +'a t

  val mu : ('a t -> 'a t) -> 'a t
  (** Generate a stream by "tying the knot"; a lazy fixed-point
      operator on streams. This can be used to make remarkably
      efficient lazy streams by enforcing structure sharing. Note
      that this function provides memoization even when using
      {!OneShot} laziness.

      For instance, [unfold (fun n -> (n, n mod 3)) 0] and [mu (fun
      s -> cons 0 (cons 1 (cons 2 s)))] are structurally identical,
      but the latter will only allocate 3 times no matter how deeply
      the stream is viewed. *)
end

module type Naperian = sig
  type +'a t
  module Totality : Wrappers.Totality.S

  val ints : int t
  (** An infinite stream of all integers. *)

  val nth : 'a t -> (int -> 'a Totality.t)
  (** Convert a stream into an accessor function on index. See
      {!tabulate}. *)

  val tabulate : (int -> 'a Totality.t) -> 'a t
  (** Generates a stream by tabulation of values. The generation
      proceeds sequentially such that the stream [tabulate f] is either
      infinite or has length equal to the {i first} [n] such that [f n]
      fails.

      Partial streams can be seen as memoizations of functions of type
      [int -> 'a option] so long as the "compaction" property above
      holds. This correspondence is nicer for {!Total} streams (see
      {!Total.tabulate}).

      See {!nth}.
  *)
end

module type Covariant = sig
  type +'a t
  val map : ('a -> 'b) -> ('a t -> 'b t)
  (** Applies a function valuewise to a stream. *)
end

module type Applicative = sig
  type +'a t

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
end

module type Comonad = sig
  type +'a t

  val extract : 'a t -> 'a
  (** Extract the focused element, e.g. [extract s = nth s 0].*)

  val extend : ('a t -> 'b) -> ('a t -> 'b t)
  (** Apply a function from streams to summary values at all points in
      time. In other words [head (extend f s) = f s] and [tail (extend f
      s) = extend f (push s)]. *)
end

module type Streaming = sig
  type +'a t

  val push : 'a t -> 'a t
  (** If a stream [s] is interpreted as a process through time then
      [delay s] is the same process beginning at {v t=-1 v} instead of
      {v t=0 v}.
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

  val interleave : 'a t -> 'a t -> 'a t
  (** Interleaves two streams, non-associative. For instance, the
      streams [let x = [1;2;3;...]] and [let y = [a;b;c;...]] are
      interwoven to form [interleave x y = [1;a;2;b;3;c;...]]. *)
end

(** A total lazy stream; a necessarily unbounded sequence of values of
    type ['a]. *)
module type Total = sig

  type +'a t
  type +'a thunk
  type +'a partial

  val cons : 'a -> 'a t -> 'a t

  val of_partial : 'a partial -> 'a t
  (** Partial streams can be extended to total streams by cycling
      them. In other words, [to_total s] is the same as [sequence s
      (sequence s ...)]. *)

  val fold : ('a -> 'r thunk -> 'r) -> ('a t -> 'r)
  (** Non-strict right fold. One must be careful in [f] about forcing
      the second argument only as needed; [fold f z s] {i can} return in
      finite time, but won't if it is too strict.

      This is the principle recursor of a total stream. Essentially any
      function eliminating streams can be derived from {!fold}.
  *)

  include Folded
    with type 'a t := 'a t
     and type 'a thunk := 'a thunk
     and module Totality = Wrappers.Totality.Pure

  include Fixed
    with type 'a t := 'a t

  include Covariant
    with type 'a t := 'a t

  include Applicative
    with type 'a t := 'a t

  include Naperian
    with type 'a t := 'a t
     and module Totality := Wrappers.Totality.Pure

  include Comonad
    with type 'a t := 'a t

  include Streaming
    with type 'a t := 'a t

end

(** A partial lazy stream; a potentially unbounded sequence of values
    of type ['a]. *)
module type Partial = sig
  type +'a t
  type +'a thunk
  type +'a total

  val empty : 'a t
  val cons : 'a -> 'a t -> 'a t

  val of_total : 'a total -> 'a t

  val of_list : 'a list -> 'a t
  (** Lists can be seen as equivalent to necessarily finite
      streams. This function injects a list into the type of partial
      streams.

      In particular, a linked list is the least fixed point of the
      functor [F X = 1 + A * X], the same functor that partial streams
      are the greatest fixed point of.
  *)

  val fold : ('a -> 'r thunk -> 'r) -> 'r -> ('a t -> 'r)
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

  val sequence : 'a t -> 'a t -> 'a t
  (** Sequences finite streams, one then the next. Streams are monoidal
      under the {!empty} stream and {!sequence}. *)

  include Folded
    with type 'a t := 'a t
     and type 'a thunk := 'a thunk
     and module Totality = Wrappers.Totality.Partial

  include Fixed
    with type 'a t := 'a t

  include Covariant
    with type 'a t := 'a t

  include Applicative
    with type 'a t := 'a t

  include Naperian
    with type 'a t := 'a t
     and module Totality := Wrappers.Totality.Partial

  include Streaming
    with type 'a t := 'a t

end
