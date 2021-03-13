open Opium
open Tyxml.Html
open Model

let () = Stdlib.Random.self_init ()
   
let myTitle = title (txt ("e-wall"))
let div = div ~a:[a_id "app"][]
let link = link ~rel:[`Stylesheet]  ~href:"ewall.css" ()
let myPage =
  html
    (head myTitle [
         script ~a:[a_src "webapp.bc.js"] (txt "");
         link
    ])
    (body [div])

let getPanel req =
  let idPanel = Router.param req "idPanel" in
  let somePanel = Panel.make ~id:idPanel ~name:"Un panneau quelconque" ~filename:"panel.png" () in
  Response.of_json (
      Panel.yojson_of_t somePanel)
  |> Lwt.return

let get_all_panels _req =
  let idPanel = "1" in
  let idPanel2 = "2" in
  let somePanel = Panel.make ~id:idPanel ~name:"Un panneau quelconque" ~filename:"panel.png" () in
  let somePanel2 = Panel.make ~id:idPanel2 ~name:"Un second panneau" ~filename:"panel2.png" () in
  Response.of_json (
      Panel.yojson_of_t_list [somePanel;somePanel2])
  |> Lwt.return

let getHold req =
  let idHold =  Router.param req "idHold" in
  let id = Uuidm.to_string @@ Uuidm.v4_gen (Stdlib.Random.get_state()) () in
  let somePanel = Panel.make ~id ~name:"Un panneau quelconque" ~filename:"panel.png" () in
  let someHold = Hold.make ~id:idHold ~panel:(somePanel) (* ~polygone:[(0,0);(1,1)] *) ~position:(10,10) ~size:10 ~name:"exemple de prise" in
  Response.of_json (Hold.yojson_of_t someHold)
  |> Lwt.return
  
let getRoute req =
  let idRoute =  Router.param req "idRoute" in
  let somePanel = Panel.make ~id:"0" ~name:"Un panneau quelconque" ~filename:"panel.png" () in
  let someHold1 = Hold.make ~id:"1" ~panel:(somePanel) (* ~polygone:[(0,0);(1,1)] *) ~position:(10,10) ~size:10 ~name:"exemple de prise" in
  let someHold2 = Hold.make ~id:"2" ~panel:(somePanel) (* ~polygone:[(0,0);(1,2)] *) ~position:(10,10) ~size:10 ~name:"exemple de prise" in
  let someRoute = Route.make
                    ~id:idRoute
                    ~name:"Papillote"
                    ~holds:[someHold1; someHold2]
                    ~feet:Route.Feet.All () in
  Response.of_json (Route.yojson_of_t someRoute)
  |> Lwt.return

let get_all_routes _req =
  let idRoute =  "1" in
  let somePanel = Panel.make ~id:"0" ~name:"Un panneau quelconque" ~filename:"panel.png" () in
  let someHold1 = Hold.make ~id:"1" ~panel:(somePanel) (* ~polygone:[(0,0);(1,1)] *) ~position:(10,10) ~size:10 ~name:"exemple de prise 1" in
  let someHold2 = Hold.make ~id:"2" ~panel:(somePanel) (* ~polygone:[(0,0);(1,2)] *) ~position:(10,10) ~size:10 ~name:"exemple de prise 2" in
  let someRoute = Route.make
                    ~id:idRoute
                    ~name:"Papillote"
                    ~holds:[someHold1; someHold2]
                    ~feet:Route.Feet.All () in
  Response.of_json (Route.yojson_of_t_list [someRoute;someRoute])
  |> Lwt.return

 
let hello2 _req = Response.of_html myPage |> Lwt.return

let greet req =
  let name = Router.param req "name" in
  Printf.sprintf "Hello, %s" name |> Response.of_plain_text |> Lwt.return



let start () =
  App.empty
  |> App.get "/" hello2
  |> App.get "/route" get_all_routes
  |> App.get "/route/:idRoute" getRoute
  |> App.get "/panel" get_all_panels
  |> App.get "/panel/:idPanel" getPanel
  |> App.get "/hold" hello2
  |> App.get "/hold/:idHold" getHold
  |> App.get "/greet/:name/" greet
  |> App.get "/greet/:name" greet
  |> App.middleware @@ Middleware.static_unix ~local_path:"_build/default/src/web-app/static" ~uri_prefix:"/" ()
  |> App.run_command
  |> ignore
