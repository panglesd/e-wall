
module Feet = struct 
  type t = All | Only [@@deriving yojson]
end

                
type t = {
    id : int option ;
    name : string ;
    holds : Hold.t list ;
    feet : Feet.t ;
    final_hold : Hold.t option ;
    initial_hold : Hold.t list option ;
    cotation : Cotation.t option ;
  } [@@deriving yojson]

type t_list = t list  [@@deriving yojson]
           
let make ?id ?initial_hold ?final_hold ?cotation ~name ~holds ~feet () = 
  {
    id ;
    name ;
    holds ;
    feet ;
    final_hold ;
    initial_hold  ;
    cotation ;
  }
