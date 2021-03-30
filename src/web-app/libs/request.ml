open Lwt.Syntax
open Js_of_ocaml_lwt.XmlHttpRequest

let get_all_panels () = 
  (* let+ is from Lwt and is equivalent to bind:
   * let+ : ('a -> 'b) -> 'a -> 'b t 
   * So this is equivalent to: 
   * - we return the promise which will :
   * - wait for perform_raw to finish
   * - apply the function list_of_json and resolve with the result *)
    let+ frame = perform_raw ~response_type:Default "panel" in
    Model.Panel.t_list_of_yojson (Yojson.Safe.from_string frame.content)

let get_panel panel_id =
  let+ frame = perform_raw ~response_type:Default ("panel/"^(string_of_int panel_id)) in
    Model.Panel.t_of_yojson (Yojson.Safe.from_string frame.content)


let get_hold hold_id =
  let+ frame = perform_raw ~response_type:Default ("hold/"^(string_of_int hold_id)) in
  Model.Hold.t_of_yojson (Yojson.Safe.from_string frame.content)
    
let get_route route_id =
  let+ frame = perform_raw ~response_type:Default ("route/"^(string_of_int route_id)) in
  Model.Route.t_of_yojson (Yojson.Safe.from_string frame.content)
        
let get_all_routes () =
  let+ frame = perform_raw ~response_type:Default ("route") in
  Model.Route.t_list_of_yojson (Yojson.Safe.from_string frame.content)
        
let get_all_holds () =
  let+ frame = perform_raw ~response_type:Default ("hold") in
  Model.Hold.t_list_of_yojson (Yojson.Safe.from_string frame.content)
        
