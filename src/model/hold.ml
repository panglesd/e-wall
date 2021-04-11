type t = {
    id : string ;
    panel : Panel.t ;
    (* polygone : (int * int) list ; *)
    position : int * int ;
    size : int ;
    name : string ;
  } [@@deriving yojson]

type t_list = t list [@@deriving yojson]

let make ~id ~panel (* ~polygone *) ~position ~size ~name =
  {
    id;
    panel;
    (* polygone : (int * int) list ; *)
    position;
    size;
    name;
  }

let make_rand_id ~panel (* ~polygone *) ~position ~size ~name =
  {
    id = Utils.get_rand_id();
    panel;
    (* polygone : (int * int) list ; *)
    position;
    size;
    name;
  }

let of_string s = s |> Yojson.Safe.from_string |> t_of_yojson
