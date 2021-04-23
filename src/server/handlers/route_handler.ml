open Opium
open Model
open Lwt.Syntax
   
let get_route req =
  let idRoute =  Router.param req "idRoute" in
  let id = Uuidm.to_string @@ Uuidm.v4_gen (Stdlib.Random.get_state()) () in
  let somePanel = Panel.make ~id:idRoute ~name:"Un panneau quelconque" ~filename:"panel.png" () in
  let someHold1 = Hold.make ~id:"1" ~panel:(somePanel) (* ~polygone:[(0,0);(1,1)] *) ~position:(10.,10.) ~size:10 ~name:"exemple de prise" in
  let someHold2 = Hold.make ~id:"2" ~panel:(somePanel) (* ~polygone:[(0,0);(1,2)] *) ~position:(10.,10.) ~size:10 ~name:"exemple de prise" in
  let someRoute = Route.make
                    ~id
                    ~name:("Papillote d'id "^id)
                    ~holds:[someHold1; someHold2]
                    ~feet:Route.Feet.All () in
  Response.of_json (Route.yojson_of_t someRoute)
  |> Lwt.return

let get_all_routes _req =
  let+ route_list = Db__Route_db.get_all () in
  Response.of_json (Route.yojson_of_t_list route_list)


let add_route req = 
  let open Opt_monad in
  let open Lwt.Syntax in
  let* _result =
    let**+ new_route_string = Request.urlencoded "new_route" req in
    new_route_string |> Yojson.Safe.from_string |> Model.Route.t_of_yojson
  in
  match _result with
    Some route ->
     let* () = Db__Route_db.add ~route in
     get_all_routes req
  | None -> get_all_routes req

let delete_route req =
  let callback = get_all_routes in
  let open Lwt.Syntax in
  let open Opt_monad in
  let* _result =
    let**+ route_to_delete_string = Request.urlencoded "route_to_delete" req in
    route_to_delete_string |> Yojson.Safe.from_string |> Model.Route.t_of_yojson
  in
  match _result with
    Some route ->
     let* _ = Db__Route_db.delete route in
     Printf.printf "Some route to remove\n";
     callback req
  | None ->
     Printf.printf "No route to remove\n";
     callback req
