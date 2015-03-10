
module Make ( Thunk : Wrappers.Thunk.S )
  : Sigs.Partial
    with type 'a t     = 'a Core.Make(Thunk).partial
     and type 'a thunk = 'a Thunk.t
     and type 'a total = 'a Core.Make(Thunk).total
= struct

  module Types    = Core.Make(Thunk)
  module Totality = Wrappers.Totality.Partial

  open Core
  open Types

  type 'a t     = 'a Types.partial
  type 'a thunk = 'a Thunk.t
  type 'a total = 'a Types.total

  let of_total = to_partial

  module Folded : Sigs.Folded
    with type 'a t       := 'a t
     and type 'a thunk   := 'a thunk
     and module Totality := Totality
  = struct

    let embed s = Partial (Thunk.map (Totality.map untup) s)
    let project (Partial s) = Totality.map tup (Thunk.force s)

    let head s = Totality.map fst (project s)
    let tail s = Totality.map snd (project s)

    let church f =
      let rec aux s = f @@ Totality.map step @@ project s
      and step (head, tail) = (head, Thunk.from_fun @@ fun () -> aux tail)
      in aux

    let unfold f =
      let rec aux s = Partial (Thunk.from_fun @@ fun () -> Totality.map wrap (f s))
      and wrap (head, s) = { head; tail = aux s }
      in aux

    let iter f = church (Totality.fold (fun (a, nx) -> f a; Thunk.force nx) ())

    let trajectory f a = unfold (fun a -> Totality.pure (a, f a)) a

    let rec impure gen = unfold (fun () -> Totality.map (fun a -> (a, ())) (gen ())) ()
  end
  include Folded

  let cons a t = embed (Thunk.from_val (Some (a, t)))
  let empty = embed (Thunk.from_val None)
  let fold f z = church (Totality.fold (fun (a, r) -> f a r) z)

  let of_list l = List.fold_right cons l empty
  let rec fold_left  snoc acc (Partial s) = fold_left_ snoc acc (Thunk.force s)
  and fold_left_ snoc acc = function
    | None -> acc
    | Some { head; tail } -> fold_left snoc (snoc acc head) tail
  let sequence = failwith "noo"

  let sequence sa (Partial sb) =
    let rec aux (Partial s) = Partial (Thunk.from_fun (aux_ (Thunk.force s)))
    and aux_ s () = Totality.fold cont (Thunk.force sb) s
    and cont { head; tail } = Some { head; tail = aux tail }
    in aux sa

  module Fixed : Sigs.Fixed
    with type 'a t := 'a t
  = struct

    let peel (Partial s) = s

    (* Ugly as sin *)
    let mu f_ =
      let rec x = lazy begin
        Partial (Thunk.from_fun begin fun () ->
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
      let rec aux (Partial s) =
        Partial begin
          Thunk.from_fun begin fun () ->
            Totality.map (bimap f aux) (Thunk.force s)
          end
        end
      in aux

  end
  include Covariant

  (* internal *)
  let rec drop_temp n (Partial s as st) =
    if n <= 0 then st
    else Totality.fold (fun x -> drop_temp (n-1) x.tail) empty (Thunk.force s)

  module Naperian : Sigs.Naperian
    with type 'a t := 'a t
    with module Totality := Totality
  = struct
    let ints = unfold (fun n -> Some (n, n+1)) 0
    let nth s n = head (drop_temp n s)
    let tabulate f = unfold (fun n -> Totality.map (fun a -> (a, n+1)) (f n)) 0
  end
  include Naperian

  module Applicative : Sigs.Applicative
    with type 'a t := 'a t
  = struct
    let pure a = unfold (fun () -> Some (a, ())) ()

    let ( >>= ) m k = Totality.bind k m

    let rec ap (Partial sf) (Partial sa) =
      Partial (Thunk.from_fun (ap_ (Thunk.force sf) (Thunk.force sa)))
    and ap_ mf ma () =
      mf >>= fun sf ->
      ma >>= fun sa ->
      Totality.pure { head = sf.head sa.head; tail = ap sf.tail sa.tail }

    let rec map2 f =
      let rec aux (Partial sx) (Partial sy) =
        Partial (Thunk.from_fun (aux_ (Thunk.force sx) (Thunk.force sy)))
      and aux_ mx my () =
        mx >>= fun sx ->
        my >>= fun sy ->
        Totality.pure { head = f sx.head sy.head
                      ; tail = aux sx.tail sy.tail
                      }
      in aux

    let rec map3 f =
      let rec aux (Partial sx) (Partial sy) (Partial sz) =
        Partial (Thunk.from_fun (aux_ (Thunk.force sx) (Thunk.force sy) (Thunk.force sz)))
      and aux_ mx my mz () =
        mx >>= fun sx ->
        my >>= fun sy ->
        mz >>= fun sz ->
        Totality.pure { head = f sx.head sy.head sz.head
                      ; tail = aux sx.tail sy.tail sz.tail
                      }
      in aux
  end
  include Applicative

  module Streaming : Sigs.Streaming
    with type 'a t := 'a t
  = struct
    let push (Partial s) = match Thunk.force s with
      | Some { tail } -> tail
      | None          -> empty

    (* With strict lists we need to build this using an accumulator to
       get tail recursion. I'm not sure if there's a faster list
       building motif. *)
    let rec take      n (Partial s) = take_ [] n (Thunk.force s)
    and     take_ acc n s =
      if n <= 0
      then List.rev acc
      else match s with
        | None -> List.rev acc
        | Some { head; tail = Partial tl} ->
          take_ (head :: acc) (n-1) (Thunk.force tl)

    let drop = drop_temp

    let inits s = ap (ap (pure take) ints) (pure s)
    let tails s = ap (ap (pure drop) ints) (pure s)

    let keep (phi : 'a -> 'b option) (s : 'a t) : 'b t =
      let folder a s =
        match phi a with
        | None   -> Thunk.force s
        | Some b -> Some { head = b; tail = Partial s }
      in Partial (Thunk.from_fun (fun () -> fold folder None s))

    let filter pred s = keep (fun a -> if pred a then Some a else None) s

    let rec interleave (Partial sa) sb =
      Partial (Thunk.from_fun (interleave_ (Thunk.force sa) sb))
    and interleave_ mx ys () =
      let ( >>= ) m k = Totality.bind k m in
      mx >>= fun xs ->
      Totality.pure { head = xs.head
                    ; tail = interleave ys xs.tail
                    }

  end
  include Streaming

end
