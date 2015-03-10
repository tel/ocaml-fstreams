
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
      if is_val t
      then from_val (f (force t))
      else from_fun (fun _ -> f (force t))
    let map2 f tx ty = match is_val tx, is_val ty with
      | true, true -> from_val (f (force tx) (force ty))
      | _ -> from_fun (fun _ -> f (force tx) (force ty))
    let pure = from_val
    let ap tf tx = match is_val tf, is_val tx with
      | true, true -> from_val (force tf (force tx))
      | _ -> from_fun (fun _ -> force tf (force tx))
    let join t =
      if is_val t then force t
      else from_fun (fun _ -> force (force t))
    let bind f t =
      if is_val t then f (force t)
      else from_fun (fun _ -> force (f (force t)))
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

module Totality = struct

  module type S = sig
    type +'a t
    type zero
    val map : ('a -> 'b) -> ('a t -> 'b t)
    val map2 : ('a -> 'b -> 'c) -> ('a t -> 'b t -> 'c t)
    val pure : 'a -> 'a t
    val ap   : ('a -> 'b) t -> ('a t -> 'b t)
    val bind : ('a -> 'b t) -> ('a t -> 'b t)
    val join : 'a t t -> 'a t
    val or_else : 'a t -> 'a t -> 'a t
    val to_option : 'a t -> 'a option
    val fold : ('a -> 'r) -> 'r -> ('a t -> 'r)
  end

  module Pure : S
    with type 'a t = 'a
     and type zero = Void.t =
  struct
    type 'a t = 'a
    type zero = Void.t
    let map f = f
    let map2 f = f
    let pure a = a
    let ap f = f
    let bind f = f
    let join x = x
    let or_else x _ = x
    let to_option x = Some x
    let fold f _ x = f x
  end

  module Partial : S
    with type 'a t = 'a option
     and type zero = unit =
  struct
    type 'a t = 'a option
    type zero = unit
    let map f = function
      | None -> None
      | Some a -> Some (f a)
    let map2 f ao bo = match ao, bo with
      | Some a, Some b -> Some (f a b)
      | _ -> None
    let pure a = Some a
    let ap fo ao = match fo, ao with
      | Some f, Some a -> Some (f a)
      | _ -> None
    let bind f = function
      | None -> None
      | Some a -> f a
    let join = function
      | None -> None
      | Some a -> a
    let or_else x y = match x with
      | None -> y
      | Some _ -> x
    let to_option x = x
    let fold f d = function
      | None -> d
      | Some a -> f a
  end

end
