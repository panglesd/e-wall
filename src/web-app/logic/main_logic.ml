open Webapp_libs
open Lwt.Syntax
open Lwd_infix   
(* The list of panels *)
let (all_panels_var:Model.Panel.t list Lwd.var) = Lwd.var [] 
(* The list of routes *)
let all_routes_var = Lwd.var [] 
(* The current route *)
let current_route_var = Lwd.var @@ None 
(* The current panel *)
let current_panel_var = Lwd.var @@ None 
let current_holds_var = Lwd.var @@ [] 
(* Whether the form for a new form should be visible  *)
(* let (panel_form_var:unit option Lwd.var) = Lwd.var @@ None *)


(** Ui state Variables  *)

type ui_state =
  Editing_Panel
| Editing_Route
| Viewing_Route_List
| Viewing_Route

let ui_state_var = Lwd.var Viewing_Route_List
let loaded = Lwd.var false
                 
let set_ui_state state = match state with
    Editing_Panel -> Lwd.set ui_state_var Editing_Panel
  | Editing_Route -> Lwd.set ui_state_var Editing_Route
  | Viewing_Route_List -> Lwd.set ui_state_var Viewing_Route_List
  | Viewing_Route -> Lwd.set ui_state_var Viewing_Route
  
let make_callback ~editing_panel ~editing_route ~viewing_route ~viewing_route_list hold_var_opt =
  let$* ui_state = Lwd.get ui_state_var in
  match ui_state with
    Editing_Panel -> editing_panel hold_var_opt
  | Editing_Route -> editing_route hold_var_opt
  | Viewing_Route_List -> viewing_route_list hold_var_opt
  | Viewing_Route -> viewing_route hold_var_opt

let get_panels_to_show =
  let$* ui_state = Lwd.get ui_state_var in
  match ui_state with
    Editing_Panel | Editing_Route | Viewing_Route_List -> Lwd.get all_panels_var
    | Viewing_Route ->
       let$ route_opt = Lwd.get current_route_var in
       match route_opt with
         None -> []
       | Some route -> 
          List.sort_uniq compare @@ List.map (fun hold -> Model.Hold.(hold.panel)) Model.Route.(route.holds)

                 
(** Global state variables, getters and setters  *)
  
let set_current_holds hold_list =
  Lwd.set current_holds_var hold_list

let update_current_holds () =
  let+ all_holds = Request.get_all_holds () in
  Lwd.set current_holds_var (List.map Lwd.var all_holds)

let save_current_holds new_holds =
  let+ all_holds = Request.send_new_holds new_holds  in
  Lwd.set current_holds_var (List.map Lwd.var all_holds)
  
  
let remove_hold hold_var =
  set_current_holds (
      List.filter (fun v -> Lwd.peek v <> Lwd.peek hold_var) @@ Lwd.peek current_holds_var
    )

let set_current_panel (panel:Model.Panel.t) =
  Lwd.set current_panel_var (Some panel)

let set_panel_list (panel_list:Model.Panel.t list) =
  Lwd.set all_panels_var panel_list;
  ignore Opt_monad.(
    let= current_panel = List.nth_opt panel_list 0 in
    set_current_panel current_panel
  )

let update_panel_list () =
  let* all_panels = Request.get_all_panels () in
  print_endline("got all panels!");
  Lwd.set all_panels_var all_panels;
  Lwd.set current_panel_var (Some (List.hd all_panels));
  update_current_holds ()

let set_current_route (route:Model.Route.t option) =
  match route with
    None ->
     Lwd.set current_route_var None;
     (* set_current_holds [];
      * ignore @@ update_panel_list () *)
  | Some route ->
     Lwd.set current_route_var (Some route)
     (* set_current_holds (List.map (fun h -> Lwd.var h) route.holds); *)
     (* set_panel_list @@ List.sort_uniq compare @@ List.map (fun hold -> Model.Hold.(hold.panel)) route.holds *)
  (* Lwd.set all_panels_var (      
   *     List.sort_uniq compare @@ List.map (fun hold -> Model.Hold.(hold.panel)) route.holds
   *   ) *)
  
let set_route_list (route_list:Model.Route.t list) =
  Lwd.set all_routes_var route_list;
  Lwd.set current_route_var (None)
  (* ignore Opt_monad.(
   *   let= current_route = List.nth_opt route_list 0 in
   *   set_current_route current_route
   * ) *)

let update_route_list () = 
  let+ all_routes = Request.get_all_routes () in
  set_route_list all_routes
  
