
module Feet = struct 
  type t = All | Only [@@deriving yojson]
end

                
type t = {
    id : string ;
    name : string ;
    holds : Hold.t list ;
    feet : Feet.t ;
    final_hold : Hold.t option ;
    initial_hold : Hold.t list option ;
    cotation : Cotation.t option ;
  } [@@deriving yojson]

type t_list = t list  [@@deriving yojson]
           
let make ~id ?initial_hold ?final_hold ?cotation ~name ~holds ~feet () = 
  {
    id ;
    name ;
    holds ;
    feet ;
    final_hold ;
    initial_hold  ;
    cotation ;
  }


let add_hold route hold =
  let id = route.id
  and initial_hold = route.initial_hold
  and final_hold = route.final_hold
  and cotation = route.cotation
  and name = route.name
  and holds = route.holds @ [hold]
  and feet = route.feet
  in
  make ~id ?initial_hold ?final_hold ?cotation ~name ~holds ~feet ()

let set_name route name =
  let id = route.id
  and initial_hold = route.initial_hold
  and final_hold = route.final_hold
  and cotation = route.cotation
  and name = name
  and holds = route.holds
  and feet = route.feet
  in
  make ~id ?initial_hold ?final_hold ?cotation ~name ~holds ~feet ()
