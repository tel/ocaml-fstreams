(** Finite streams *)
type 'a t

val head       : 'a t -> 'a option
val tail       : 'a t -> 'a t option
val uncons     : 'a t -> ('a * 'a t) option
val cons       : 'a -> 'a t -> 'a t
val empty      : 'a t
    
(** Interleaves two streams *)
val alt        : 'a t -> 'a t -> 'a t

(** Sequences finite streams one then the next *)
val seq        : 'a t -> 'a t -> 'a t

(** Non-strict right fold. If [f] is careful about forcing its second
    argument only as needed then [fold f z s] can return in finite
    time even when [s] is infinite. *)
val fold       : ('a -> 'r Lazy.t -> 'r) -> 'r -> ('a t -> 'r Lazy.t)

(** Strict left fold. If [s] is infinite then [fold_left f z s] will
    never terminate. The advantage is that [fold_left] is
    tail-recursive. *)
val fold_left  : ('r -> 'a -> 'r) -> 'r -> ('a t -> 'r)
val unfold     : ('s -> ('a * 's) option) -> ('s -> 'a t)
val trajectory : ('a -> 'a) -> ('a -> 'a t)
val of_list    : 'a list -> 'a t
val map        : ('a -> 'b) -> ('a t -> 'b t)
val ints       : int t
val tabulate   : (int -> 'a) -> 'a t
val pure       : 'a -> 'a t
val ap         : ('a -> 'b) t -> ('a t -> 'b t)
val take       : int -> 'a t -> 'a list
val drop       : int -> 'a t -> 'a t
val inits      : 'a t -> 'a list t
val tails      : 'a t -> 'a t t
val nth        : int -> 'a t -> 'a option
val keep       : ('a -> 'b option) -> ('a t -> 'b t)
val filter     : ('a -> bool) -> ('a t -> 'a t)
