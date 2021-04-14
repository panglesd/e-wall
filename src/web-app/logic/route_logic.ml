(* open Tyxml_lwd.Html *)
(* open Model *)
(* open Lwd_infix *)

let create_route_callback : Tyxml_lwd.Xml.mouse_event_handler =
  let f = fun _e ->
    let blank_route = Model__Route.make ~id:(Utils.get_rand_id ()) ~name:"fzefzef" ~holds:[] ~feet:Model.Route.Feet.Only () in
    Lwd.set Main_logic.ui_state_var Main_logic.Editing_Route;
    Lwd.set Main_logic.current_route_var (Some blank_route);
    false in
  Lwd.pure @@ Some f

let close_route_callback : Tyxml_lwd.Xml.mouse_event_handler =
  let f = fun _ ->
    Main_logic.set_current_route None;
    Lwd.set Main_logic.ui_state_var Main_logic.Viewing_Route_List;
    false in
  Lwd.pure @@ Some f

let save_route : Tyxml_lwd.Xml.mouse_event_handler =
  let f = fun _e ->
    match Lwd.peek Main_logic.current_route_var with
      None -> false
    | Some route ->
       ignore @@ Webapp_libs.Request.send_new_route route;
       false in
  Lwd.pure (Some f)

  
let mouse_click_callback hold_var_opt : Tyxml_lwd.Xml.mouse_event_handler =
  let f hold_var = fun _e ->
    let current_route_opt = Lwd.peek Main_logic.current_route_var in
    match current_route_opt with
      None -> false
    | Some current_route ->
       Lwd.set Main_logic.current_route_var (Some (Model.Route.add_hold current_route (Lwd.peek hold_var)));
       false
  in
  match hold_var_opt with
    None -> Lwd.pure None
  | Some hold_var ->
     Lwd.pure @@ Some (f hold_var)

(* let mouse_click_callback hold_var_opt =
 *   let$* tool = Lwd.get tool_var in
 *   match tool with
 *     Move -> Lwd.pure None
 *   | Add -> mouse_click_add
 *   | Delete -> match hold_var_opt with None -> Lwd.pure None | Some hold_var ->  mouse_click_remove hold_var *)
                                                                              
                                                                              
(* let mouse_move_move : Tyxml_lwd.Xml.mouse_event_handler =
 *   () *)
  
(* let mouse_move_callback _hold_var_opt =
 *   let$* tool = Lwd.get tool_var in
 *   match tool with
 *     Move -> mouse_move_move
 *   | Add -> Lwd.pure None
 *   | Delete -> Lwd.pure None *)

(* let mouse_up_move : Tyxml_lwd.Xml.mouse_event_handler =
 *   let$ focused = Lwd.get focused_var in
 *   match focused with
 *     None -> None
 *   | Some _ ->
 *      Some (fun _e -> Lwd.set focused_var None; false) *)

(* let mouse_up_callback _hold_var_opt = 
 *   let$* tool = Lwd.get tool_var in
 *   match tool with
 *     Move -> mouse_up_move
 *   | Add -> Lwd.pure None
 *   | Delete -> Lwd.pure None *)

(* let mouse_down_move hold_var =
 *   Lwd.pure @@
 *     Some (fun e ->
 *         let xev,yev = Js_of_ocaml.Dom_html.eventAbsolutePosition e in
 *         let x,y = Model.Hold.((Lwd.peek hold_var).position) in
 *         Lwd.set focused_var (Some (hold_var, (x, y), (xev,yev))); false) *)
  
let mouse_down_callback hold_var_opt =
  match hold_var_opt with
    None -> Lwd.pure None
  | Some _hold_var ->
     Lwd.pure None
    
let mouse_leave_move _hold_var =
  Lwd.pure @@
    Some (fun _e ->
        false)
  
let mouse_leave_callback _hold_var = 
  ()                
                
