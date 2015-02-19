(** Functional, lazy streams.

    A very standard infinite data structure is the stream which lazily
    determines its current and next values and even whether they exist or
    not! This data structure is useful for describing computations which
    are generated and consumed step-by-step and is a powerful basic tool
    for many lazy algorithms.

    Included in this module are two submodules, Infinite and
    Finite. Infinite streams are streams which are guaranteed to always
    have a next value and Finite streams are streams which may terminate
    but are not obliged to.

    Mathematically, Infinite streams are the greatest fixed point of the
    functor F X = A * X and Finite streams are the greatest fixed point of
    the functor F X = 1 + A * X.

*)

(** Abstract laziness. *)
module type Lz = sig
  type +'a t
  val force    : 'a t -> 'a
  val from_fun : (unit -> 'a) -> 'a t
  val from_val : 'a -> 'a t
end

module Lz = struct

  (** Memoized laziness replaces forced values by their realizations
      possibly saving large amounts of recomputation. *)
  module Memoized : Lz with type 'a t = 'a Lazy.t = Lazy 

  (** One-shot laziness performs no memoization. It can be much faster
      than {!Memoized} so long as values are only accessed once. *)
  module OneShot : Lz with type 'a t = unit -> 'a = struct
    type 'a t = unit -> 'a
    let force f = f ()
    let from_fun f = f
    let from_val v = fun () -> v 
  end
  
end

(** The impossible type: no values can be made to exist.

    Values of type {!t} cannot be made to exist, there are no
    introduction forms. This makes them the impossible type; however,
    they are not without their use. A function which claims to return
    {!t} must actually never return. A hypothetical situation which
    offers a value of {!t} must be {!absurd}.

*)
module Void = Void

module Make (Lz : Lz) = struct

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
      
      Mathematically, total streams are the greatest fixed point of
      the functor [F X = A * X].
      
  *)
  type 'a total = 'a total_node Lz.t
  and  'a total_node = { head : 'a; tail : 'a total }

  type 'a partial = 'a partial_node Lz.t
  and 'a partial_node =
    | Cons of 'a * 'a partial
    | Empty
  
  module type Total = sig
    
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
        
    val of_partial : 'a partial -> 'a t
    (** Partial streams can be extended to total streams by cycling
        them. In other words, [to_total s] is the same as [sequence s
        (sequence s ...)]. *)
        
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
                         
    val fold : ('a -> 'r Lz.t -> 'r) -> ('a t -> 'r)
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
      type 'a t = 'a total
      and 'a node = 'a total_node
    end
    
  end

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
  module type Partial = sig
    
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
        
    val of_total : 'a total -> 'a t
    (** As partial streams may be infinite we can inject total streams
        into them. *)
        
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
        
    val fold : ('a -> 'r Lz.t -> 'r) -> 'r -> ('a t -> 'r)
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
        
    val sequence : 'a t -> 'a t -> 'a t
    (** Sequences finite streams, one then the next. Streams are monoidal
        under the {!empty} stream and {!sequence}. *)
        
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
      type 'a t = 'a partial
      and 'a node = 'a partial_node
    end
    
  end

  module Total : Total = struct
    
    type 'a t = 'a total
        
    module Impl = struct
      type 'a t = 'a total
      and  'a node = 'a total_node
    end
    
    let rec uncons  q = uncons_ (Lz.force q)
    and     uncons_ s = (s.head, s.tail)
                        
    let head s = fst (uncons s)
    let tail s = snd (uncons s)
        
    let cons head tail = Lz.from_val { head; tail }
        
    let rec interleave  sa sb    = Lz.from_fun (interleave_ (Lz.force sa) sb)
    and     interleave_ xs ys () = { head = xs.head; tail = interleave ys xs.tail }
                                
    let rec fold  cons s    = fold_ cons (Lz.force s) ()
    and     fold_ cons s () = cons s.head (Lz.from_fun (fold_ cons (Lz.force s.tail)))
        
    let rec unfold  phi s    = Lz.from_fun (unfold_ phi s)
    and     unfold_ phi s () = let (head, s) = phi s in { head; tail = unfold phi s }

    let of_partial par =
      let rec go1 p () = go2 (Lz.force p)
      and     go2 p    = match p with
        | Empty -> go1 par ()
        | Cons (head, tail) -> { head; tail = Lz.from_fun (go1 tail) }
      in Lz.from_fun (go1 par)
    
    let trajectory endo x = unfold (fun x -> (x, endo x)) x
        
    let impure gen = unfold (fun s -> (gen s, s)) ()
        
    let rec iter eff s =
      let { head; tail } = Lz.force s in
      eff head; iter eff tail
        
    let rec map  f s    = Lz.from_fun (map_ f (Lz.force s))
    and     map_ f s () = { head = f s.head; tail = map f s.tail }
                       
    let ints       = unfold (fun n -> (n, n+1)) 0
    let tabulate f = map f ints
    let pure a     = tabulate (fun _ -> a)
        
    let rec ap  fs xs    = Lz.from_fun (ap_ (Lz.force fs) (Lz.force xs))
    and     ap_ fs xs () = { head = fs.head xs.head; tail = ap fs.tail xs.tail }
                        
    let rec map2  f xs ys    = Lz.from_fun (map2_ f (Lz.force xs) (Lz.force ys))
    and     map2_ f xs ys () = { head = f xs.head ys.head; tail = map2 f xs.tail ys.tail }
                            
    let rec map3  f xs ys zs = Lz.from_fun (map3_ f (Lz.force xs) (Lz.force ys) (Lz.force zs))
    and     map3_ f xs ys zs () =
      { head = f xs.head ys.head zs.head
      ; tail = map3 f xs.tail ys.tail zs.tail
      }
                               
    (* With strict lists we need to build this using an accumulator to
       get tail recursion. I'm not sure if there's a faster list
       building motif. *)
    let take n s =
      let rec aux  acc n s = aux_ acc n (Lz.force s)
      and     aux_ acc n s =
        if n <= 0 then List.rev acc else aux (s.head :: acc) (n-1) s.tail
      in aux [] n s
        
    let rec drop n s =
      if n <= 0 then s else drop (n-1) (Lz.force s).tail
          
    let inits s = ap (ap (pure take) ints) (pure s)
    let tails s = ap (ap (pure drop) ints) (pure s)
        
    let nth n s = head (drop n s)
        
    let keep phi s =
      let folder a r = match phi a with
        | None   -> Lz.force r
        | Some b -> { head = b; tail = r }
      in Lz.from_fun (fun () -> fold folder s)
        
    let filter pred s = keep (fun a -> if pred a then Some a else None) s
        
    let extract  s = (Lz.force s).head
    let extend f s = map f (tails s)
        
    let push s = tail s
        
  end
  
  module Partial : Partial = struct
    
    type 'a t = 'a partial
        
    module Impl = struct
      type 'a t = 'a partial
      and 'a node = 'a partial_node
    end
    
    let rec uncons q = match Lz.force q with
      | Empty -> None
      | Cons (hd, tl) -> Some (hd, tl)
        
    let head s = Option.map fst (uncons s)
    let tail s = Option.map snd (uncons s)
        
    let cons a tl = Lz.from_val (Cons (a, tl))
    let empty     = Lz.from_val Empty
        
    let rec interleave  sa sb    = Lz.from_fun (interleave_ (Lz.force sa) sb)
    and     interleave_ xs ys () = match xs with
      | Empty         -> Lz.force ys
      | Cons (x, xs') -> Cons (x, interleave ys xs')
                           
    let rec sequence  sa sb    = Lz.from_fun (sequence_ (Lz.force sa) sb)
    and     sequence_ sa sb () = match sa with
      | Empty         -> Lz.force sb
      | Cons (x, xs') -> Cons (x, sequence xs' sb)
                           
    let rec fold  cons empty s    = fold_ cons empty (Lz.force s) ()
    and     fold_ cons empty p () = match p with
      | Empty        -> empty
      | Cons (a, tl) -> cons a (Lz.from_fun (fold_ cons empty (Lz.force tl)))
                          
    let rec fold_left  snoc acc s = fold_left_ snoc acc (Lz.force s)
    and     fold_left_ snoc acc   = function
      | Empty        -> acc
      | Cons (a, tl) -> fold_left snoc (snoc acc a) tl
                          
    let rec iter eff ?finally:(finally = fun x -> x) s =
      match Lz.force s with
      | Cons (a, s_) -> eff a; iter eff s_
      | Empty -> finally ()
                  
    let rec unfold  phi s    = Lz.from_fun (unfold_ phi s)
    and     unfold_ phi s () = match phi s with
      | None         -> Empty
      | Some (a, s_) -> Cons (a, unfold phi s_)
                          
    let trajectory endo x = unfold (fun x -> Some (x, endo x)) x
        
    let impure gen =
      unfold (fun s -> Option.map (fun a -> (a, s)) (gen s)) ()
        
    let rec of_list  l    = Lz.from_fun (of_list_ l)
    and     of_list_ l () = match l with
      | []      -> Empty
      | h :: tl -> Cons (h, of_list tl)

    let rec of_total tot = Lz.from_fun (aux (Lz.force tot))
    and aux { head; tail } () = Cons (head, of_total tail)
    
    let rec map  f s    = Lz.from_fun (map_ f (Lz.force s))
    and     map_ f s () = match s with
      | Empty        -> Empty
      | Cons (a, tl) -> Cons (f a, map f tl)
                          
    let ints       = unfold (fun n -> Some (n, n+1)) 0
    let tabulate f = unfold (fun n -> Option.map (fun a -> (a, n+1)) (f n)) 0
    let pure a     = tabulate (fun _ -> Some a)
        
    let rec ap  fs xs    = Lz.from_fun (ap_ (Lz.force fs) (Lz.force xs))
    and     ap_ fs xs () = match fs, xs with
      | Cons (f, fs_), Cons (x, xs_) -> Cons (f x, ap fs_ xs_)
      | _                            -> Empty
        
    let rec map2  f xs ys    = Lz.from_fun (map2_ f (Lz.force xs) (Lz.force ys))
    and     map2_ f xs ys () = match xs, ys with
      | Cons (x, xs_), Cons (y, ys_) -> Cons (f x y, map2 f xs_ ys_)
      | _                            -> Empty
        
    let rec map3  f xs ys zs =
      Lz.from_fun (map3_ f (Lz.force xs) (Lz.force ys) (Lz.force zs))
    and     map3_ f xs ys zs () = match xs, ys, zs with
      | Cons (x, xs_), Cons (y, ys_), Cons (z, zs_) ->
        Cons (f x y z, map3 f xs_ ys_ zs_)
      | _ -> Empty
        
    (* With strict lists we need to build this using an accumulator to
       get tail recursion. I'm not sure if there's a faster list
       building motif. *)
    let rec take      n s = take_ [] n (Lz.force s)
    and     take_ acc n s =
      if n <= 0
      then List.rev acc
      else match s with
        | Empty        -> List.rev acc
        | Cons (a, tl) -> take_ (a :: acc) (n-1) (Lz.force tl)
                            
    let rec drop  n s    = Lz.from_fun (drop_ n (Lz.force s))
    and     drop_ n s () =
      if n <= 0
      then Empty
      else match s with
        | Empty        -> Empty
        | Cons (_, tl) -> drop_ (n-1) (Lz.force tl) ()
                            
    let inits s = ap (ap (pure take) ints) (pure s)
    let tails s = ap (ap (pure drop) ints) (pure s)
        
    let rec nth  s n = nth_ (Lz.force s) n
    and     nth_ s n = match s with
      | Empty        -> None
      | Cons (h, tl) -> if n <= 0 then Some h else nth tl (n-1)
            
    let keep phi s =
      let folder a r = match phi a with
        | None  -> Lz.force r
        | Some b -> Cons (b, r)
      in Lz.from_fun (fun () -> fold folder Empty s)
        
    let filter pred = keep (fun a -> if pred a then Some a else None)
        
    let push s = match Lz.force s with
      | Cons (a, tl) -> tl
      | Empty        -> empty

  end
  
end


module Memoized = Make (Lz.Memoized)
module OneShot  = Make (Lz.OneShot)

include Memoized
