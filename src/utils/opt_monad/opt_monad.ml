let ( let** ) o f = match o with
  | None -> None
  | Some x -> f x
let ( and** ) a b = match a,b with
  | None, _ -> None
  | _, None -> None
  | Some x, Some y -> Some (x,y) 

let ( let= ) o f = match o with
  | None -> None
  | Some x -> Some (f x)
let ( and= ) a b = match a,b with
  | None, _ -> None
  | _, None -> None
  | Some x, Some y -> Some (x,y) 

let ( *=< ) f a =
  let** f = f in
  f a
let ( <=* ) f a =
  let** a = a in
  f a
  
let ( *=* ) f a =
  let** f = f
  and** a = a in
  f a


let ( let*+ ) a f =
  Lwt.Syntax.(
    let* a_opt = a in
    match a_opt with
      None -> Lwt.return None
    | Some a -> f a )
let ( and*+ ) a b =
  Lwt.Syntax.(
    let+ a_opt = a and+ b_opt = b in
    let** a = a_opt and** b = b_opt in
    Some (a,b)
  )

  
let ( let**+ ) a f =
  let open Lwt.Syntax in
  let+ a_opt = a in
  let= a = a_opt in
  f a
(* let ( and*+ ) a b =
 *   let**+ a = a in
 *   let**+ b = b in
 *   (a,b) *)

    
let ( and**+ ) = ( and*+ )
