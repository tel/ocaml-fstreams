
module Impl = Internal.Total
include Impl

let rec uncons  q = uncons_ (Lazy.force q)
and     uncons_ s = (s.head, s.tail)

let head s = fst (uncons s)
let tail s = snd (uncons s)

let cons head tail = lazy { head; tail }

let rec interleave  sa sb = lazy (interleave_ (Lazy.force sa) sb)
and     interleave_ xs ys = { head = xs.head; tail = interleave ys xs.tail }

let rec fold  cons s = fold_ cons (Lazy.force s)
and     fold_ cons s = cons s.head (lazy (fold_ cons (Lazy.force s.tail)))

let rec unfold  phi s = lazy (unfold_ phi s)
and     unfold_ phi s = let (head, s) = phi s in { head; tail = unfold phi s }

let trajectory endo x = unfold (fun x -> (x, endo x)) x

let impure gen = unfold (fun s -> (gen s, s)) ()

let rec iter eff s =
  let { head; tail } = Lazy.force s in
  eff head; iter eff tail

let rec map  f s = lazy (map_ f (Lazy.force s))
and     map_ f s = { head = f s.head; tail = map f s.tail }

let ints       = unfold (fun n -> (n, n+1)) 0
let tabulate f = map f ints
let pure a     = tabulate (fun _ -> a)

let rec ap  fs xs = lazy (ap_ (Lazy.force fs) (Lazy.force xs))
and     ap_ fs xs = { head = fs.head xs.head; tail = ap fs.tail xs.tail }

let rec map2  f xs ys = lazy (map2_ f (Lazy.force xs) (Lazy.force ys))
and     map2_ f xs ys = { head = f xs.head ys.head; tail = map2 f xs.tail ys.tail }

let rec map3  f xs ys zs = lazy (map3_ f (Lazy.force xs) (Lazy.force ys) (Lazy.force zs))
and     map3_ f xs ys zs = { head = f xs.head ys.head zs.head
                           ; tail = map3 f xs.tail ys.tail zs.tail
                           }

(* With strict lists we need to build this using an accumulator to
   get tail recursion. I'm not sure if there's a faster list
   building motif. *)
let take n s =
  let rec aux  acc n s = aux_ acc n (Lazy.force s)
  and     aux_ acc n s =
    if n <= 0 then List.rev acc else aux (s.head :: acc) (n-1) s.tail
  in aux [] n s

let rec drop n s =
  if n <= 0 then s else drop (n-1) (Lazy.force s).tail
  
let inits s = ap (ap (pure take) ints) (pure s)
let tails s = ap (ap (pure drop) ints) (pure s)

let nth n s = head (drop n s)

let keep phi s =
  let folder a r = match phi a with
    | None   -> Lazy.force r
    | Some b -> { head = b; tail = r }
  in lazy (fold folder s)

let filter pred s = keep (fun a -> if pred a then Some a else None) s

let extract  s = (Lazy.force s).head
let extend f s = map f (tails s)

let push s = tail s
