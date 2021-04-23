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
        
let send_new_holds new_holds =
  let yojsoned = new_holds |> Model.Hold.yojson_of_t_list |> Yojson.Safe.to_string in
  let+ frame = perform_raw_url ~contents:(`POST_form([("new_holds", `String (Js_of_ocaml.Js.string yojsoned))])) "hold" in
  Model.Hold.t_list_of_yojson (Yojson.Safe.from_string frame.content)
  (* print_endline frame.content;
   * get_all_holds () *)

let delete_panel panel =
  let yojsoned = panel |> Model.Panel.yojson_of_t |> Yojson.Safe.to_string in
  let+ frame = perform_raw_url ~contents:(`POST_form([("panel_to_delete", `String (Js_of_ocaml.Js.string yojsoned))])) "delete/panel" in
  Model.Panel.t_list_of_yojson (Yojson.Safe.from_string frame.content)

let send_new_route new_route =
  let yojsoned = new_route |> Model.Route.yojson_of_t |> Yojson.Safe.to_string in
  let* frame = perform_raw_url ~contents:(`POST_form([("new_route", `String (Js_of_ocaml.Js.string yojsoned))])) "route" in
  Lwt.return @@ Model.Route.t_list_of_yojson (Yojson.Safe.from_string frame.content)
  (* print_endline frame.content;
   * get_all_holds () *)
                  
let delete_route new_route =
  let yojsoned = new_route |> Model.Route.yojson_of_t |> Yojson.Safe.to_string in
  let* frame = perform_raw_url ~contents:(`POST_form([("route_to_delete", `String (Js_of_ocaml.Js.string yojsoned))])) "delete/route" in
  Lwt.return @@ Model.Route.t_list_of_yojson (Yojson.Safe.from_string frame.content)
