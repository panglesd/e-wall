type t = {
    id : int option ;
    name : string ;
  } [@@deriving yojson]

let make ?id ~name = {
    id ;
    name;
  }
