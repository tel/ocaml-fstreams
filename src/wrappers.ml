
module Thunk = struct

  module type Basis = sig
    type +'a t
    val force    : 'a t -> 'a
    val from_fun : (unit -> 'a) -> 'a t
    val from_val : 'a -> 'a t
    val is_val   : 'a t -> bool
  end

  module type S = sig
    include Basis
    val map  : ('a -> 'b) -> ('a t -> 'b t)
    val map2 : ('a -> 'b -> 'c) -> ('a t -> 'b t -> 'c t)
    val pure : 'a -> 'a t
    val ap   : ('a -> 'b) t -> ('a t -> 'b t)
    val bind : ('a -> 'b t) -> ('a t -> 'b t)
    val join : 'a t t -> 'a t
  end

  module Make ( B : Basis ) : S
    with type 'a t = 'a B.t =
  struct
    include B
    let map f t =
      if B.is_val t
      then B.from_val (f (B.force t))
      else B.from_fun (fun _ -> f (B.force t))
    let map2 f tx ty = match B.is_val tx, B.is_val ty with
      | true, true -> B.from_val (f (B.force tx) (B.force ty))
      | _ -> B.from_fun (fun _ -> f (B.force tx) (B.force ty))
    let pure = B.from_val
    let ap tf tx = match B.is_val tf, B.is_val tx with
      | true, true -> B.from_val (B.force tf (B.force tx))
      | _ -> B.from_fun (fun _ -> B.force tf (B.force tx))
    let join t =
      if B.is_val t then B.force t
      else B.from_fun (fun _ -> B.force (B.force t))
    let bind f t =
      if B.is_val t then f (B.force t)
      else B.from_fun (fun _ -> B.force (f (B.force t)))
  end

  (** Memoized laziness replaces forced values by their realizations
      possibly saving large amounts of recomputation at the cost of
      an extra indirection and reference for each node in the stream.

      This is just an alias for the system Lazy module. *)
  module Memoized : S
    with type 'a t = 'a Lazy.t
    = Make (Lazy)

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
