
module Make (Thunk : Wrappers.Thunk_s) = struct

  type ('a, 'x) node =
    { head : 'a
    ; tail : 'x
    }

  type 'a total   = Total   of ('a, 'a total)   node        Thunk.t
  type 'a partial = Partial of ('a, 'a partial) node option Thunk.t

  let to_total (s0 : 'a partial) : 'a total =
    let aux (Partial s) = Thunk

end

module type Void = sig

  type t
  (** The nonexistent data type. *)

  val absurd : t -> 'a
  (** It is possible in some hypothetical contexts to have access to a
      value of type {!t}. As no values of {!t} can ever come into
      existence we resolve that our hypotheses are wrong and thus
      conclude whatever we like.

      In pithier words, {i from nothing comes anything}.
  *)
end

module Void : Void = struct

  type t = { t : 'a . 'a }
  (* The absurdity of [t] is self-evident. The only "natural" way to
     produce a value of [t] is to have a value of literally any type
     with no constraint. *)

  let absurd t = t.t

end
