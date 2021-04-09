open Opium
open Model
open Lwt.Syntax

let get_hold req =
  let id_hold =  Router.param req "idHold" in
  let* hold = Db__Hold_db.get ~id:id_hold in
  (* let id = Uuidm.to_string @@ Uuidm.v4_gen (Stdlib.Random.get_state()) () in
   * let somePanel = Panel.make ~id ~name:("Un panneau quelconque d'id "^id) ~filename:"panel.png" () in
   * let someHold = Hold.make ~id:idHold ~panel:(somePanel) (\* ~polygone:[(0,0);(1,1)] *\) ~position:(10,10) ~size:10 ~name:"exemple de prise" in *)
  Response.of_json (Hold.yojson_of_t hold)
  |> Lwt.return

let get_all_holds _req =
  let open Lwt.Syntax in
  let+ panel_list = Db__Hold_db.get_all () in
  Response.of_json (
      Hold.yojson_of_t_list panel_list)

  

let add_hold_yojsoned req =
  let open Opt_monad in
  let callback = Main_handler.main_handler in
  let+ hold_opt = Request.urlencoded "hold_jsoned" req in
  let _ =
    let** hold_string = hold_opt in
    let hold =  hold_string
                |> Yojson.Safe.from_string
                |> Model.Hold.t_of_yojson in
    Some (Db__Hold_db.set ~hold:hold) in
  callback req



let update_holds req = 
  let open Opt_monad in
  let open Lwt.Syntax in
  let* _result =
    let**+ new_holds_string = Request.urlencoded "new_holds" req in
    new_holds_string |> Yojson.Safe.from_string |> Model.Hold.t_list_of_yojson
  in
  match _result with
    Some hold_list ->
     let+ () = Db__Hold_db.set_all hold_list in
     Response.of_plain_text "dzdzd"
  | None -> Response.of_plain_text "yoooooooooooo" |> Lwt.return
  

  
(* let add_hold req =
 * 
 *   let open Opt_monad in
 *          
 *   let callback = Main_handler.main_handler in
 * 
 *   let* _result =
 *     let**+ hold =
 *       let**+ panel_string = Request.urlencoded "panel" req
 *       and**+ position_x_string = Request.urlencoded "position_x" req
 *       and**+ position_y_string = Request.urlencoded "position_y" req
 *       and**+ size_string = Request.urlencoded "size" req
 *       and**+ name_string = Request.urlencoded "name" req in
 *       Model.Hold.make
 *               ~id:"0"
 *               ~panel:(Panel.of_string panel_string)
 *               ~position:(int_of_string position_x_string, int_of_string position_y_string)
 *               ~size:(int_of_string size_string)
 *               ~name:name_string
 *     in
 *     let+ result = Db__Hold_db.add ~hold in
 *     result
 *   in
 * 
 *   callback req *)
