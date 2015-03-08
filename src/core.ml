
type ('a, 'x) node =
  { head : 'a
  ; tail : 'x
  }

let lmap f n = { n with head = f n.head }
let rmap f n = { n with tail = f n.tail }

module type S = sig

  type 'a total
  type 'a partial

  val to_total   : 'a partial -> 'a total
  val to_partial : 'a total -> 'a partial

end

module Make (Thunk : Wrappers.Thunk.S) : S = struct

  type 'a total   = Total   of ('a, 'a total)   node        Thunk.t
  type 'a partial = Partial of ('a, 'a partial) node option Thunk.t

  let to_total (Partial s0 as p : 'a partial) : 'a total =
    let rec roll : ('a, 'a partial) node option -> ('a, 'a total) node = function
      | None -> roll (Thunk.force s0)
      | Some node -> rmap aux node
    and aux (Partial s) = Total (Thunk.map roll s)
    in aux p

  let to_partial (t : 'a total) : 'a partial =
    let rec roll n = Some (rmap aux n)
    and aux (Total s) = Partial (Thunk.map roll s)
    in aux t

end
