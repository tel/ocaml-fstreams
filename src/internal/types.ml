
module MakeStreams (Lazy : Wrappers.Thunk_s) = struct

  type 'a partial = unit -> 'a partial_node
  and 'a partial_node =
    | Nil
    | Cons of 'a * 'a partial

  type 'a total = unit -> 'a total_node
  and 'a total_node =
    { head : 'a
    ; tail : 'a total
    }

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
