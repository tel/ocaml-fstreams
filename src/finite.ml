type 'a t = 'a q Lazy.t
 and 'a q = 
   | Cons of 'a * 'a t
   | Empty

let rec uncons q = uncons_ (Lazy.force q)
and uncons_ = function
  | Empty        -> None
  | Cons (a, lt) -> Some (a, lt)

let head s = Option.map fst (uncons s)
let tail s = Option.map snd (uncons s)

let cons a tl = lazy (Cons (a, tl))
let empty     = lazy Empty

let rec alt  sa sb = lazy (alt_ (Lazy.force sa) sb)
and     alt_ xs ys = match xs with
  | Empty         -> Lazy.force ys
  | Cons (x, xs') -> Cons (x, alt ys xs')

let rec seq  sa sb = lazy (seq_ (Lazy.force sa) sb)
and     seq_ sa sb = match sa with
  | Empty         -> Lazy.force sb
  | Cons (x, xs') -> Cons (x, seq xs' sb)

let rec fold  cons empty s = lazy (fold_ cons empty (Lazy.force s))
and     fold_ cons empty   = function
  | Empty        -> empty
  | Cons (a, tl) -> cons a (fold cons empty tl)

let rec fold_left  snoc acc s = fold_left_ snoc acc (Lazy.force s)
and     fold_left_ snoc acc   = function
  | Empty        -> acc
  | Cons (a, tl) -> fold_left snoc (snoc acc a) tl

let rec unfold  phi s = lazy (unfold_ phi s)
and     unfold_ phi s = match phi s with
  | None         -> Empty
  | Some (a, s_) -> Cons (a, unfold phi s_)

let trajectory endo x = unfold (fun x -> Some (x, endo x)) x

let rec of_list  l = lazy (of_list_ l)
and     of_list_   = function
  | []      -> Empty
  | h :: tl -> Cons (h, of_list tl)
  
let rec map  f s = lazy (map_ f (Lazy.force s))
and     map_ f   = function
  | Empty        -> Empty
  | Cons (a, tl) -> Cons (f a, map f tl)

let ints       = unfold (fun n -> Some (n, n+1)) 0
let tabulate f = map f ints
let pure a     = tabulate (fun _ -> a)

let rec ap  fs xs = lazy (ap_ (Lazy.force fs) (Lazy.force xs))
and     ap_ fs xs = match fs, xs with
  | Cons (f, fs_), Cons (x, xs_) -> Cons (f x, ap fs_ xs_)
  | _            , _             -> Empty

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
