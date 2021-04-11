open Opium
open Ewall_handlers
       

let test = (fun req ->
    let open Opt_monad in
    let open Lwt.Syntax in
    let+ _result =
        let**+ _panel_string = Request.urlencoded "new_holds" req in
        _panel_string
    in
    match _result with
      Some s ->
       let hold_list = s |> Yojson.Safe.from_string |> Model.Hold.t_list_of_yojson in
       ignore @@ List.map (fun e -> e) hold_list;
       Response.of_plain_text s
    | None -> Response.of_plain_text "yoooooooooooo"
  )
 

let start () =
  App.empty
  |> App.middleware Middleware.debugger
  |> App.post "/panel" @@ Panel_handler.add_panel
  |> App.post "/hold" @@ Hold_handler.update_holds
  |> App.post "/route" @@ Route_handler.add_route
  |> App.get "/" Main_handler.main_handler
  |> App.get "/route" Route_handler.get_all_routes
  |> App.get "/route/:idRoute" Route_handler.get_route
  |> App.get "/panel" Panel_handler.get_all_panels
  |> App.get "/panel/:idPanel" Panel_handler.get_panel
  |> App.get "/hold" Hold_handler.get_all_holds
  |> App.get "/hold/:idHold" Hold_handler.get_hold
  |> App.post "/test" test
  |> App.middleware @@ Middleware.static_unix ~local_path:".ewall/img/panel-img" ~uri_prefix:"/img/panel-img" ()
  |> App.middleware @@ Middleware.static_unix ~local_path:"_build/default/src/web-app/static" ~uri_prefix:"/" ()
  |> App.run_command
  |> ignore
