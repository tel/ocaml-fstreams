
module Thunk = struct

  module type S = sig
    type +'a t
    val force    : 'a t -> 'a
    val from_fun : (unit -> 'a) -> 'a t
    val from_val : 'a -> 'a t
    val map  : ('a -> 'b) -> ('a t -> 'b t)
    val map2 : ('a -> 'b -> 'c) -> ('a t -> 'b t -> 'c t)
    val pure : 'a -> 'a t
    val ap   : ('a -> 'b) t -> ('a t -> 'b t)
    val bind : ('a -> 'b t) -> ('a t -> 'b t)
    val join : 'a t t -> 'a t
  end

  (** Memoized laziness replaces forced values by their realizations
      possibly saving large amounts of recomputation at the cost of
      an extra indirection and reference for each node in the stream.

      This is just an alias for the system Lazy module. *)
  module Memoized : S
    with type 'a t = 'a Lazy.t =
  struct
    include Lazy
    let map f t =
      if Lazy.is_val t
      then Lazy.from_val (f (Lazy.force t))
      else Lazy.from_fun (fun _ -> f (Lazy.force t))
    let map2 f tx ty = match Lazy.is_val tx, Lazy.is_val ty with
      | true, true -> Lazy.from_val (f (Lazy.force tx) (Lazy.force ty))
      | _ -> Lazy.from_fun (fun _ -> f (Lazy.force tx) (Lazy.force ty))
    let pure = Lazy.from_val
    let ap tf tx = match Lazy.is_val tf, Lazy.is_val tx with
      | true, true -> Lazy.from_val (Lazy.force tf (Lazy.force tx))
      | _ -> Lazy.from_fun (fun _ -> Lazy.force tf (Lazy.force tx))
    let join t =
      if Lazy.is_val t then Lazy.force t
      else Lazy.from_fun (fun _ -> Lazy.force (Lazy.force t))
    let bind f t =
      if Lazy.is_val t then f (Lazy.force t)
      else Lazy.from_fun (fun _ -> Lazy.force (f (Lazy.force t)))
  end

  (** One-shot laziness performs no memoization. It can be much faster
      than memoized laziness so long as values are only accessed once. *)
  module Simple : S
    with type 'a t = unit -> 'a =
  struct
    type 'a t = unit -> 'a
    let force f = f ()
    let from_fun f = f
    let from_val v = fun () -> v
    let is_val v = false

    let map f t = fun () -> f (t ())
    let pure x = fun () -> x
    let map2 f tx ty = fun () -> f (tx ()) (ty ())
    let ap tf tx = fun () -> tf () (tx ())
    let join t = fun () -> t () ()
    let bind f t = fun () -> f (t ()) ()
  end

end
