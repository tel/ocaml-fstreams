
(** The signature of modules providing lazy deferrals. Both of the
    submodules of {!module:Lz} instantiate it. *)
module type Thunk_s = sig
  type +'a t
  val force    : 'a t -> 'a
  val from_fun : (unit -> 'a) -> 'a t
  val from_val : 'a -> 'a t
end

(** Memoized laziness replaces forced values by their realizations
    possibly saving large amounts of recomputation at the cost of
    an extra indirection and reference for each node in the stream.

    This is just an alias for the system Lazy module. *)
module MemoizedThunk : Thunk_s with type 'a t := 'a Lazy.t = Lazy

(** One-shot laziness performs no memoization. It can be much faster
    than memoized laziness so long as values are only accessed once. *)
module SimpleThunk : Thunk_s with type 'a t = unit -> 'a = struct
  type 'a t = unit -> 'a
  let force f = f ()
  let from_fun f = f
  let from_val v = fun () -> v
end
