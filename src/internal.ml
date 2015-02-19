
module rec Total : sig

  type 'a t = 'a q Lazy.t
  and 'a q = { head : 'a; tail : 'a t }

  val of_partial : 'a Partial.t -> 'a t
  
end = struct
  
  type 'a t = 'a q Lazy.t
  and 'a q = { head : 'a; tail : 'a t }

  let rec of_partial par =
    let rec go1 p = go2 (Lazy.force p)
    and     go2 p = match p with
      | Partial.Empty -> go1 par
      | Partial.Cons (head, tail) -> { head; tail = lazy (go1 tail) }
    in lazy (go1 par)

end

and Partial : sig

  type 'a t = 'a q Lazy.t
  and 'a q = 
    | Cons of 'a * 'a t
    | Empty

  val of_total : 'a Total.t -> 'a t
  
end = struct

  type 'a t = 'a q Lazy.t
  and 'a q = 
    | Cons of 'a * 'a t
    | Empty

  let rec of_total tot = lazy (aux (Lazy.force tot))
  and aux { Total.head; Total.tail } = Cons (head, of_total tail)
  
end
