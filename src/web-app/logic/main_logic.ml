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


  
let get_panels_to_show =
  let$* ui_state = Ui_logic.get_ui_state in
  match ui_state with
    Editing_Panel | Editing_Route | Viewing_Route_List -> Lwd.get all_panels_var
    | Viewing_Route ->
       let$ route_opt = Lwd.get current_route_var in
       match route_opt with
         None -> []
       | Some route -> 
          List.sort_uniq compare @@ List.map (fun hold -> Model.Hold.(hold.panel)) Model.Route.(route.holds)

                 
(* Getters, for Lwd and value *)
let get_all_panels = Lwd.get all_panels_var
let get_all_routes = Lwd.get all_routes_var
let get_current_route = Lwd.get current_route_var
let get_current_panel = Lwd.get current_panel_var
let get_current_holds = Lwd.get current_holds_var

let get_all_panels_val () = Lwd.peek all_panels_var
let get_all_routes_val () = Lwd.peek all_routes_var
let get_current_route_val () = Lwd.peek current_route_var
let get_current_panel_val () = Lwd.peek current_panel_var
let get_current_holds_val () = Lwd.peek current_holds_var


                      
(* Setters *)
let set_current_panel (panel:Model.Panel.t) =
  Lwd.set current_panel_var (Some panel)
  
let set_panel_list (panel_list:Model.Panel.t list) =
  Lwd.set all_panels_var panel_list;
  ignore Opt_monad.(
    let= current_panel = List.nth_opt panel_list 0 in
    set_current_panel current_panel
  )

let set_route_list (route_list:Model.Route.t list) =
  Lwd.set all_routes_var route_list;
  Lwd.set current_route_var (None)
  
let set_current_route (route:Model.Route.t option) =
  match route with
    None ->
     Lwd.set current_route_var None;
  | Some route ->
     Lwd.set current_route_var (Some route)
    
let set_current_holds hold_list =
  Lwd.set current_holds_var hold_list

let add_hold hold =
  Lwd.set current_holds_var @@ (Lwd.var hold)::(Lwd.peek current_holds_var)
  
(* Receive from server *)
let update_current_holds () =
  let+ all_holds = Request.get_all_holds () in
  Lwd.set current_holds_var (List.map Lwd.var all_holds)

let update_panel_list () =
  let* all_panels = Request.get_all_panels () in
  print_endline("got all panels!");
  Lwd.set all_panels_var all_panels;
  Lwd.set current_panel_var (Some (List.hd all_panels));
  update_current_holds ()

let update_route_list () = 
  let+ all_routes = Request.get_all_routes () in
  set_route_list all_routes

(* Send to server *)
  
let save_current_holds new_holds =
  let+ all_holds = Request.send_new_holds new_holds  in
  Lwd.set current_holds_var (List.map Lwd.var all_holds)
  
let remove_hold hold_var =
  set_current_holds (
      List.filter (fun v -> Lwd.peek v <> Lwd.peek hold_var) @@ Lwd.peek current_holds_var
    )




  

  
