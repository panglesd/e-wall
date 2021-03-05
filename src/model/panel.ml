type t = {
    id : int option ;
    name : string ;
    filename: string
  } [@@deriving yojson]

type t_list = t list  [@@deriving yojson]
       
let make ?id ~name ~filename () = {
    id ;
    name;
    filename;
  }
