open !Opium
open Model


let get_all_panels _req =
  let open Lwt.Syntax in
  let* panel_list = Db__Panel_db.get_all () in
  Response.of_json (
      Panel.yojson_of_t_list panel_list)
  |> Lwt.return

let get_panel req =
  let idPanel = Router.param req "idPanel" in
  let somePanel = Panel.make ~id:idPanel ~name:"Un panneau quelconque" ~filename:"panel.png" () in
  Response.of_json (
      Panel.yojson_of_t somePanel)
  |> Lwt.return

let add_panel req =
  let callback = Main_handler.main_handler in
  let open Lwt.Syntax in
  let open Opt_monad in
  let panel_id = Utils.get_rand_id () in
  let* panel_map_list_opt, filenames = Server_utils.Utils.save_file ~prefix:panel_id ~folder:".ewall/img/panel-img" req in 
  let filename = match filenames with [] -> None | fn::_ -> Some fn in
  match filename with None -> callback req | Some filename ->
  let new_panel =
    let id = panel_id in
    let** panel_map_list = panel_map_list_opt in
    let get s = let rec aux u = match u with (a,b) :: _ when a = s -> Some b | _::t -> aux t | [] -> None in aux panel_map_list in
    let** panel_name = get "panel_name" in
    Some (Model.Panel.make ~id ~name:panel_name ~filename ()) in
  match new_panel with
    None ->      print_string "No new panel :/";flush_all();
                 callback req
  | Some new_panel ->
     print_string @@ Model.Panel.pp new_panel;flush_all();
     let* () = Db__Panel_db.add ~panel:new_panel in
     callback req
