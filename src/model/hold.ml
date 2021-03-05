type t = {
    id : int option ;
    panel : Panel.t ;
    (* polygone : (int * int) list ; *)
    position : int * int ;
    size : int ;
    name : string ;
  } [@@deriving yojson]


let make ?id ~panel (* ~polygone *) ~position ~size ~name =
  {
    id;
    panel;
    (* polygone : (int * int) list ; *)
    position;
    size;
    name;
  }
