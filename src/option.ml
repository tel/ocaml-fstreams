
module Base = struct
  type 'a t = 'a option

  let fold some none = function
    | Some a -> some a
    | None   -> none
      
  let map f = function
    | Some a -> Some (f a)
    | None   -> None
      
  let pure a = Some a
      
  let bind k = function
    | Some a -> k a
    | None   -> None
      
  let ap f a =
    bind (fun f_ -> bind (fun a_ -> pure (f_ a_)) a) f

  let bind k = function
    | Some a -> k a
    | None   -> None
  
  let zero = None
end

include Base

module First = struct
  let prod a b = match a with
    | Some _ -> a
    | None   -> b
end

module Last = struct
  let prod a b = match b with
    | Some _ -> b
    | None   -> a
end
