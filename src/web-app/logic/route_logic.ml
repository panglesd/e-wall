(* open Tyxml_lwd.Html *)
(* open Model *)
(* open Lwd_infix *)
open Lwt.Syntax

let create_route_callback : Tyxml_lwd.Xml.mouse_event_handler =
  let f = fun _e ->
    let blank_route = Model__Route.make ~id:(Utils.get_rand_id ()) ~name:"" ~holds:[] ~feet:Model.Route.Feet.Only () in
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
       let _ =
         let+ route_list = Webapp_libs.Request.send_new_route route in
         Main_logic.set_route_list route_list;
         Lwd.set Main_logic.ui_state_var Main_logic.Viewing_Route_List in
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
                
let set_current_route_name name =
  match Lwd.peek Main_logic.current_route_var with
    None -> ()
  | Some route ->
     Lwd.set Main_logic.current_route_var (Some (Model.Route.set_name route name))


let onchange_name_callback : Tyxml_lwd.Xml.event_handler =
  Lwd.pure @@
    Some (
        fun e ->
        let target_js_opt = e##.currentTarget in
        let target_opt = Js_of_ocaml.Js.Opt.to_option target_js_opt in
        match target_opt with
          None -> false
        | Some target ->
           match  Js_of_ocaml.Js.Opt.to_option @@ Js_of_ocaml.Dom_html.CoerceTo.input target  with
             None -> false
           | Some input_target ->
              let input_value = Js_of_ocaml.Js.to_string input_target##.value in
              set_current_route_name input_value;
              false
      )


let select_route_callback route =
    Some (fun _e ->
        Main_logic.set_current_route (Some route);
        Lwd.set Main_logic.ui_state_var Main_logic.Viewing_Route;
        false
      )
