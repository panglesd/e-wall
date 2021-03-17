open Opium
open Model

let get_hold req =
  let idHold =  Router.param req "idHold" in
  let id = Uuidm.to_string @@ Uuidm.v4_gen (Stdlib.Random.get_state()) () in
  let somePanel = Panel.make ~id ~name:("Un panneau quelconque d'id "^id) ~filename:"panel.png" () in
  let someHold = Hold.make ~id:idHold ~panel:(somePanel) (* ~polygone:[(0,0);(1,1)] *) ~position:(10,10) ~size:10 ~name:"exemple de prise" in
  Response.of_json (Hold.yojson_of_t someHold)
  |> Lwt.return
