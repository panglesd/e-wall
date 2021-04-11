open Opium
open Model

let get_route req =
  let idRoute =  Router.param req "idRoute" in
  let id = Uuidm.to_string @@ Uuidm.v4_gen (Stdlib.Random.get_state()) () in
  let somePanel = Panel.make ~id:idRoute ~name:"Un panneau quelconque" ~filename:"panel.png" () in
  let someHold1 = Hold.make ~id:"1" ~panel:(somePanel) (* ~polygone:[(0,0);(1,1)] *) ~position:(10,10) ~size:10 ~name:"exemple de prise" in
  let someHold2 = Hold.make ~id:"2" ~panel:(somePanel) (* ~polygone:[(0,0);(1,2)] *) ~position:(10,10) ~size:10 ~name:"exemple de prise" in
  let someRoute = Route.make
                    ~id
                    ~name:("Papillote d'id "^id)
                    ~holds:[someHold1; someHold2]
                    ~feet:Route.Feet.All () in
  Response.of_json (Route.yojson_of_t someRoute)
  |> Lwt.return

let get_all_routes _req =
  (* let idRoute =  "1" in *)
  let id = Uuidm.to_string @@ Uuidm.v4_gen (Stdlib.Random.get_state()) () in
  let somePanel = Panel.make ~id:"0" ~name:"Un panneau quelconque" ~filename:"panel.png" () in
  let someHold1 = Hold.make ~id:"1" ~panel:(somePanel) (* ~polygone:[(0,0);(1,1)] *) ~position:(10,10) ~size:10 ~name:"exemple de prise 1" in
  let someHold2 = Hold.make ~id:"2" ~panel:(somePanel) (* ~polygone:[(0,0);(1,2)] *) ~position:(10,10) ~size:10 ~name:"exemple de prise 2" in
  let someRoute = Route.make
                    ~id
                    ~name:("Papillote d'id "^id)
                    ~holds:[someHold1; someHold2]
                    ~feet:Route.Feet.All () in
  let id = Uuidm.to_string @@ Uuidm.v4_gen (Stdlib.Random.get_state()) () in
  let someRoute2 = Route.make
                     ~id
                     ~name:("Papillote 2 d'id "^id)
                     ~holds:[someHold1; someHold2]
                     ~feet:Route.Feet.All () in
  Response.of_json (Route.yojson_of_t_list [someRoute;someRoute2])
  |> Lwt.return


let add_route req = 
  let open Opt_monad in
  let open Lwt.Syntax in
  let* _result =
    let**+ new_route_string = Request.urlencoded "new_route" req in
    new_route_string |> Yojson.Safe.from_string |> Model.Route.t_of_yojson
  in
  match _result with
    Some route ->
     let+ () = Db__Route_db.add ~route in
     Response.of_plain_text "dzdzd"
  | None -> Response.of_plain_text "yoooooooooooo" |> Lwt.return
