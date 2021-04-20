type t = {
    id : string ;
    panel : Panel.t ;
    (* polygone : (int * int) list ; *)
    position : float * float ;
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

let set_name hold name =
  make ~id:hold.id
    ~panel:hold.panel
    ~position:hold.position
    ~size:hold.size
    ~name

let set_size hold size =
  make ~id:hold.id
    ~panel:hold.panel
    ~position:hold.position
    ~size:size
    ~name:hold.name
