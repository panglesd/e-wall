open Opium
open Model
open Ewall_handlers
       

  
 

let start () =
  App.empty
  |> App.middleware Middleware.debugger
  |> App.post "/panel" @@ Panel_handler.add_panel
  |> App.get "/" Main_handler.main_handler
  |> App.get "/route" Route_handler.get_all_routes
  |> App.get "/route/:idRoute" Route_handler.get_route
  |> App.get "/panel" Panel_handler.get_all_panels
  |> App.get "/panel/:idPanel" Panel_handler.get_panel
  |> App.get "/hold" Main_handler.main_handler
  |> App.get "/hold/:idHold" Hold_handler.get_hold
  |> App.middleware @@ Middleware.static_unix ~local_path:".ewall/img/panel-img" ~uri_prefix:"/img/panel-img" ()
  |> App.middleware @@ Middleware.static_unix ~local_path:"_build/default/src/web-app/static" ~uri_prefix:"/" ()
  |> App.run_command
  |> ignore
