type t = {
    id : string ;
    name : string ;
    filename: string
  } [@@deriving yojson]

type t_list = t list  [@@deriving yojson]
       
let make ~id ~name ~filename () = {
    id ;
    name;
    filename;
  }
