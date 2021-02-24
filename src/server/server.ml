open Opium
open Tyxml.Html
open Model
   
let myTitle = title (txt ("e-wall"))
let div = div ~a:[a_id "app"][txt "hello, my worlds!"]
let link = link ~rel:[`Stylesheet]  ~href:"ewall.css" ()
let myPage =
  html
    (head myTitle [
         script ~a:[a_src "webapp.bc.js"] (txt "");
         link
    ])
    (body [div])
   
let getPanel req =
  let idPanel = int_of_string @@ Router.param req "idPanel" in
  let somePanel = Panel.make ~id:idPanel ~name:"Un panneau quelconque" in
  Response.of_json (
      Panel.yojson_of_t somePanel)
  |> Lwt.return

let getHold req =
  let idHold =  int_of_string @@ Router.param req "idHold" in
  let somePanel = Panel.make ~id:0 ~name:"Un panneau quelconque" in
  let someHold = Hold.make ~id:idHold ~panel:(somePanel) ~polygone:[(0,0);(1,1)] ~name:"exemple de prise" in
  Response.of_json (Hold.yojson_of_t someHold)
  |> Lwt.return
  
let getRoute req =
  let idRoute =  int_of_string @@ Router.param req "idRoute" in
  let somePanel = Panel.make ~id:0 ~name:"Un panneau quelconque" in
  let someHold1 = Hold.make ~id:1 ~panel:(somePanel) ~polygone:[(0,0);(1,1)] ~name:"exemple de prise" in
  let someHold2 = Hold.make ~id:2 ~panel:(somePanel) ~polygone:[(0,0);(1,2)] ~name:"exemple de prise" in
  let someRoute = Route.make
                    ~id:idRoute
                    ~name:"Papillote"
                    ~holds:[someHold1; someHold2]
                    ~feet:Route.Feet.All () in
  Response.of_json (Route.yojson_of_t someRoute)
  |> Lwt.return
  
let hello2 _req = Response.of_html myPage |> Lwt.return

let greet req =
  let name = Router.param req "name" in
  Printf.sprintf "Hello, %s" name |> Response.of_plain_text |> Lwt.return



let start () =
  App.empty
  |> App.get "/" hello2
  |> App.get "/route" hello2
  |> App.get "/route/:idRoute" getRoute
  |> App.get "/panel" hello2
  |> App.get "/panel/:idPanel" getPanel
  |> App.get "/hold" hello2
  |> App.get "/hold/:idHold" getHold
  |> App.get "/greet/:name/" greet
  |> App.get "/greet/:name" greet
  |> App.middleware @@ Middleware.static_unix ~local_path:"_build/default/src/web-app/static" ~uri_prefix:"/" ()
  |> App.run_command
  |> ignore
