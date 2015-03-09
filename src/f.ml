
module Total ( Thunk : Wrappers.Thunk.S )
  : Sigs.Total
    with type 'a t       = 'a Core.Make(Thunk).total
     and type 'a thunk   = 'a Thunk.t
     and type 'a partial = 'a Core.Make(Thunk).partial
= struct

  module Types    = Core.Make(Thunk)
  module Totality = Wrappers.Totality.Pure

  open Core
  open Types

  type 'a t       = 'a Types.total
  type 'a thunk   = 'a Thunk.t
  type 'a partial = 'a Types.partial

  let of_partial = to_total

  module Folded : Sigs.Folded
    with type 'a t       := 'a t
     and type 'a thunk   := 'a thunk
     and module Totality := Totality
  = struct

    module Totality = Totality

    type 'a t       = 'a Types.total
    type 'a thunk   = 'a Thunk.t

    let embed t = Total (Thunk.map (fun (head, tail) -> {head; tail}) t)
    let project (Total s) =
      let { head; tail } = Thunk.force s in (head, tail)

    let head s = fst (project s)
    let tail s = snd (project s)

    let church f =
      let rec aux (Total s) =
        let node = Thunk.force s in
        f (node.head, Thunk.from_fun (fun _ -> aux node.tail))
      in aux

    let unfold f s =
      let rec aux s0 =
        let (head, s1) = f s in
        Total (
          Thunk.from_fun (fun () -> { head; tail = aux s1 })
        )
      in aux s

    let iter f =
      let rec aux (Total s) =
        let n = Thunk.force s in
        f n.head; aux n.tail
      in aux

    let trajectory f a =
      let rec aux head =
        Total (Thunk.from_fun (fun () -> { head; tail = aux (f head) }))
      in aux a

    let rec impure gen =
      let rec aux () =
        Total (Thunk.from_fun (fun () -> { head = gen (); tail = aux () }))
      in aux ()
  end
  include Folded

  let cons a t = embed (Thunk.from_val (a, t))
  let fold f = church (fun (a, r) -> f a r)

  module Fixed : Sigs.Fixed
    with type 'a t := 'a t
  = struct

    let peel (Total s) = s

    (* Ugly as sin *)
    let mu f_ =
      let rec x = lazy begin
        Total (Thunk.from_fun begin fun () ->
          Thunk.force (peel (f_ (Lazy.force x)))
        end)
      end
      in Lazy.force x
  end
  include Fixed

  module Covariant : Sigs.Covariant
    with type 'a t := 'a t
  = struct

    let map f =
      let rec aux (Total s) =
        Total (Thunk.from_fun (aux_ (Thunk.force s)))
      and aux_ { head; tail } () =
        { head = f head; tail = aux tail }
      in aux

  end
  include Covariant

  (* internal *)
  let rec drop_temp n (Total s as st) =
    if n <= 0 then st else drop_temp (n-1) (Thunk.force s).tail

  module Naperian : Sigs.Naperian
    with type 'a t := 'a t
    with module Totality := Totality
  = struct
    let ints = unfold (fun n -> (n, n+1)) 0
    let nth s n = head (drop_temp n s)
    let tabulate f = map f ints
  end
  include Naperian

  module Applicative : Sigs.Applicative
    with type 'a t := 'a t
  = struct
    let pure a =
      let rec aux () =
        embed (Thunk.from_fun (fun () -> (a, aux ())))
      in aux ()

    let rec ap (Total sf) (Total sa) =
      Total (Thunk.from_fun (ap_ (Thunk.force sf) (Thunk.force sa)))
    and ap_ sf sa () = { head = sf.head sa.head; tail = ap sf.tail sa.tail }

    let rec map2 f =
      let rec aux (Total sx) (Total sy) =
        Total (Thunk.from_fun (aux_ (Thunk.force sx) (Thunk.force sy)))
      and aux_ sx sy () =
        { head = f sx.head sy.head
        ; tail = aux sx.tail sy.tail
        }
      in aux

    let rec map3 f =
      let rec aux (Total sx) (Total sy) (Total sz) =
        Total (Thunk.from_fun (aux_ (Thunk.force sx) (Thunk.force sy) (Thunk.force sz)))
      and aux_ sx sy sz () =
        { head = f sx.head sy.head sz.head
        ; tail = aux sx.tail sy.tail sz.tail
        }
      in aux
  end
  include Applicative

  module Streaming : Sigs.Streaming
    with type 'a t := 'a t
  = struct
    let push = tail

    (* With strict lists we need to build this using an accumulator to
       get tail recursion. I'm not sure if there's a faster list
       building motif. *)
    let take n s =
      let rec aux  acc n (Total s) = aux_ acc n (Thunk.force s)
      and     aux_ acc n s =
        if n <= 0 then List.rev acc else aux (s.head :: acc) (n-1) s.tail
      in aux [] n s

    let drop = drop_temp

    let inits s = ap (ap (pure take) ints) (pure s)
    let tails s = ap (ap (pure drop) ints) (pure s)

    let keep (phi : 'a -> 'b option) (s : 'a t) : 'b t =
      let folder ((a, s) : 'a * ('b, 'b t) node Thunk.t) : ('b, 'b t) node =
        match phi a with
        | None   -> Thunk.force s
        | Some b -> { head = b; tail = Total s }
      in Total (Thunk.from_fun (fun () -> church folder s))

    let filter pred s = keep (fun a -> if pred a then Some a else None) s

    let rec interleave (Total sa) sb =
      Total (Thunk.from_fun (interleave_ (Thunk.force sa) sb))
    and interleave_ xs ys () =
      { head = xs.head
      ; tail = interleave ys xs.tail
      }

  end
  include Streaming

  module Comonad : Sigs.Comonad
    with type 'a t := 'a t
  = struct
    let extract (Total s) =
      let { head } = Thunk.force s in head

    let extend f s = map f (tails s)
  end
  include Comonad

end
