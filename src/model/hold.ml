type t = {
    id : int option ;
    panel : Panel.t ;
    polygone : (int * int) list ;
    name : string ;
  } [@@deriving yojson]


let make ?id ~panel ~polygone ~name =
  {
    id : int option ;
    panel : Panel.t ;
    polygone : (int * int) list ;
    name : string ;
  }
