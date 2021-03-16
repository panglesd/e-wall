open Opium
open Tyxml.Html
open Model

let () = Stdlib.Random.self_init ()
let get_rand_id () =
  let _ = Stdlib.Random.bits () in
  Uuidm.to_string @@ Uuidm.v4_gen (Stdlib.Random.get_state()) ()
  
       
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
  let open Lwt.Syntax in
  (* let idPanel = "1" in
   * let idPanel2 = "2" in
   * let somePanel = Panel.make ~id:idPanel ~name:"Un panneau quelconque" ~filename:"panel.png" () in
   * let somePanel2 = Panel.make ~id:idPanel2 ~name:"Un second panneau" ~filename:"panel2.png" () in *)
  let* panel_list = Db__Panel_db.get_all () in
  Response.of_json (
      Panel.yojson_of_t_list panel_list)
  |> Lwt.return

let getHold req =
  let idHold =  Router.param req "idHold" in
  let id = Uuidm.to_string @@ Uuidm.v4_gen (Stdlib.Random.get_state()) () in
  let somePanel = Panel.make ~id ~name:("Un panneau quelconque d'id "^id) ~filename:"panel.png" () in
  let someHold = Hold.make ~id:idHold ~panel:(somePanel) (* ~polygone:[(0,0);(1,1)] *) ~position:(10,10) ~size:10 ~name:"exemple de prise" in
  Response.of_json (Hold.yojson_of_t someHold)
  |> Lwt.return
  
let getRoute req =
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

 
let hello2 _req = Response.of_html myPage |> Lwt.return

let greet req =
  let name = Router.param req "name" in
  Printf.sprintf "Hello, %s" name |> Response.of_plain_text |> Lwt.return

let save_file ~prefix ~folder req =
  let open Lwt.Syntax in 
  let files = Hashtbl.create ~random:true 5 in
  let callback ~name:_ ~filename string =
    let filepath = folder^"/"^prefix^(Filename.basename filename) in
    let write file =
      string |> String.length |> Lwt_unix.write_string file string 0 |> Lwt.map ignore
    in
    match Hashtbl.find_opt files filepath with
    | Some file -> write file
    | None ->
      let* file =
        Lwt_unix.openfile filepath Unix.[ O_CREAT; O_TRUNC; O_WRONLY; O_NONBLOCK ] 0o600
      in
      Hashtbl.add files filepath file;
      write file
  in
  let* tab = Request.to_multipart_form_data ~callback req in
  let close filepath file prev =
    let* l = prev in
    let* () = Lwt_unix.close file in
    let new_size = (Unix.stat filepath).st_size - 2 in
    Unix.truncate filepath new_size;
    Lwt.return @@ (Filename.basename filepath) :: l
  in
  let+ filenames = Hashtbl.fold close files (Lwt.return []) in
   (tab, filenames)

    (* print_endline string; *)
  
let add_panel req =
  (* Request.pp_hum (Format.std_formatter) req; *)
  let open Lwt.Syntax in
  let open Opt_monad in
  let panel_id = get_rand_id () in
  let* panel_map_list_opt, filenames = save_file ~prefix:panel_id ~folder:".ewall/img/panel-img" req in 
  let filename = match filenames with [] -> None | fn::_ -> Some fn in
  match filename with None -> hello2 req | Some filename ->
  let new_panel =
    let id = panel_id in
    let** panel_map_list = panel_map_list_opt in
    (* List.iter (fun (s,t) -> print_string @@ Printf.sprintf "%s is mapped to %s\n" s t) panel_map_list;
     * print_endline (fst @@ List.hd panel_map_list);
     * print_endline (snd @@ List.hd panel_map_list); *)
    let get s = let rec aux u = match u with (a,b) :: _ when a = s -> Some b | _::t -> aux t | [] -> None in aux panel_map_list in
    let** panel_name = get "panel_name" in
    Some (Model.Panel.make ~id ~name:panel_name ~filename ()) in
  match new_panel with
    None ->      print_string "No new panel :/";flush_all();
hello2 req
  | Some new_panel ->
     print_string @@ Model.Panel.pp new_panel;flush_all();
     let* () = Db__Panel_db.add ~panel:new_panel in
     hello2 req
     (* let* e = Request.to_multipart_form_data req in
      * match e with
      *   Some _ ->
      *   Printf.sprintf "Creating panel, %s %s %s\n" new_panel.id new_panel.name new_panel.filename |> Response.of_plain_text |> Lwt.return
      *  | None ->
      *   Printf.sprintf "Not Creating panel, %s %s %s\n" new_panel.id new_panel.name new_panel.filename |> Response.of_plain_text |> Lwt.return *)

let start () =
  App.empty
  |> App.middleware Middleware.debugger
  |> App.post "/panel" add_panel
  |> App.get "/" hello2
  |> App.get "/route" get_all_routes
  |> App.get "/route/:idRoute" getRoute
  |> App.get "/panel" get_all_panels
  |> App.get "/panel/:idPanel" getPanel
  |> App.get "/hold" hello2
  |> App.get "/hold/:idHold" getHold
  |> App.get "/greet/:name/" greet
  |> App.get "/greet/:name" greet
  |> App.middleware @@ Middleware.static_unix ~local_path:".ewall/img/panel-img" ~uri_prefix:"/img/panel-img" ()
  |> App.middleware @@ Middleware.static_unix ~local_path:"_build/default/src/web-app/static" ~uri_prefix:"/" ()
  |> App.run_command
  |> ignore
