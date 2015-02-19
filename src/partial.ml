module Impl = Internal.Partial
include Impl

let rec uncons q = uncons_ (Lazy.force q)
and uncons_ = function
  | Empty        -> None
  | Cons (a, lt) -> Some (a, lt)

let head s = Option.map fst (uncons s)
let tail s = Option.map snd (uncons s)

let cons a tl = lazy (Cons (a, tl))
let empty     = lazy Empty

let rec interleave  sa sb = lazy (interleave_ (Lazy.force sa) sb)
and     interleave_ xs ys = match xs with
  | Empty         -> Lazy.force ys
  | Cons (x, xs') -> Cons (x, interleave ys xs')

let rec sequence  sa sb = lazy (sequence_ (Lazy.force sa) sb)
and     sequence_ sa sb = match sa with
  | Empty         -> Lazy.force sb
  | Cons (x, xs') -> Cons (x, sequence xs' sb)

let rec fold  cons empty s = fold_ cons empty (Lazy.force s)
and     fold_ cons empty   = function
  | Empty        -> empty
  | Cons (a, tl) -> cons a (lazy (fold_ cons empty (Lazy.force tl)))

let rec fold_left  snoc acc s = fold_left_ snoc acc (Lazy.force s)
and     fold_left_ snoc acc   = function
  | Empty        -> acc
  | Cons (a, tl) -> fold_left snoc (snoc acc a) tl

let rec iter eff ?finally:(finally = fun x -> x) s =
  match Lazy.force s with
  | Cons (a, s_) -> eff a; iter eff s_
  | Empty -> finally ()

let rec unfold  phi s = lazy (unfold_ phi s)
and     unfold_ phi s = match phi s with
  | None         -> Empty
  | Some (a, s_) -> Cons (a, unfold phi s_)

let trajectory endo x = unfold (fun x -> Some (x, endo x)) x

let impure gen =
  unfold (fun s -> Option.map (fun a -> (a, s)) (gen s)) ()

let rec of_list  l = lazy (of_list_ l)
and     of_list_   = function
  | []      -> Empty
  | h :: tl -> Cons (h, of_list tl)
  
let rec map  f s = lazy (map_ f (Lazy.force s))
and     map_ f   = function
  | Empty        -> Empty
  | Cons (a, tl) -> Cons (f a, map f tl)

let ints       = unfold (fun n -> Some (n, n+1)) 0
let tabulate f = unfold (fun n -> Option.map (fun a -> (a, n+1)) (f n)) 0
let pure a     = tabulate (fun _ -> Some a)

let rec ap  fs xs = lazy (ap_ (Lazy.force fs) (Lazy.force xs))
and     ap_ fs xs = match fs, xs with
  | Cons (f, fs_), Cons (x, xs_) -> Cons (f x, ap fs_ xs_)
  | _            , _             -> Empty

let rec map2  f xs ys = lazy (map2_ f (Lazy.force xs) (Lazy.force ys))
and     map2_ f xs ys = match xs, ys with
  | Cons (x, xs_), Cons (y, ys_) -> Cons (f x y, map2 f xs_ ys_)
  | _            , _             -> Empty

let rec map3  f xs ys zs = lazy (map3_ f (Lazy.force xs) (Lazy.force ys) (Lazy.force zs))
and     map3_ f xs ys zs = match xs, ys, zs with
  | Cons (x, xs_), Cons (y, ys_), Cons (z, zs_) ->
    Cons (f x y z, map3 f xs_ ys_ zs_)
  | _ -> Empty

(* With strict lists we need to build this using an accumulator to
   get tail recursion. I'm not sure if there's a faster list
   building motif. *)
let rec take      n s = take_ [] n (Lazy.force s)
and     take_ acc n s =
  if n <= 0
  then List.rev acc
  else match s with
    | Empty        -> List.rev acc
    | Cons (a, tl) -> take_ (a :: acc) (n-1) (Lazy.force tl)

let rec drop  n s = lazy (drop_ n (Lazy.force s))
and     drop_ n s =
  if n <= 0
  then Empty
  else match s with
    | Empty        -> Empty
    | Cons (_, tl) -> drop_ (n-1) (Lazy.force tl)
  
let inits s = ap (ap (pure take) ints) (pure s)
let tails s = ap (ap (pure drop) ints) (pure s)

let rec nth  s n = nth_ (Lazy.force s) n
and     nth_ s n = match s with
  | Empty        -> None
  | Cons (h, tl) -> if n <= 0 then Some h else nth tl (n-1)
      
let keep phi s =
  let folder a r = match phi a with
    | None   -> Lazy.force r
    | Some b -> Cons (b, r)
  in lazy (fold folder Empty s)

let filter pred = keep (fun a -> if pred a then Some a else None)

let push s = match Lazy.force s with
  | Cons (a, tl) -> tl
  | Empty        -> lazy Empty
