
type t
(** The nonexistent data type. *)

val absurd : t -> 'a
(** It is possible in some hypothetical contexts to have access to a
    value of type {!t}. As no values of {!t} can ever come into
    existence we resolve that our hypotheses are wrong and thus
    conclude whatever we like.

    In pithier words, {i from nothing comes anything}.
*)
