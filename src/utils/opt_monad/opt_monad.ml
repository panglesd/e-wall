let ( let** ) o f = match o with
  | None -> None
  | Some x -> f x
let ( and** ) a b = match a,b with
  | None, _ -> None
  | _, None -> None
  | Some x, Some y -> Some (x,y) 
