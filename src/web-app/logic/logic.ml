open Webapp_libs
open Lwt.Syntax
   
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





  
let set_current_panel (panel:Model.Panel.t) =
  Lwd.set current_panel_var (Some panel)

let set_panel_list (panel_list:Model.Panel.t list) =
  Lwd.set all_panels_var panel_list;
  ignore Opt_monad.(
    let= current_panel = List.nth_opt panel_list 0 in
    set_current_panel current_panel
  )

let update_panel_list () =
  let+ all_panels = Request.get_all_panels () in
  print_endline("got all panels!");
  Lwd.set all_panels_var all_panels;
  Lwd.set current_panel_var (Some (List.hd all_panels))

let set_current_route (route:Model.Route.t option) =
  match route with
    None ->
     Lwd.set current_route_var None;
     Lwd.set current_holds_var [];
     ignore @@ update_panel_list ()
  | Some route ->
     Lwd.set current_route_var (Some route);
     Lwd.set current_holds_var (List.map (fun h -> Lwd.var h) route.holds);
     set_panel_list @@ List.sort_uniq compare @@ List.map (fun hold -> Model.Hold.(hold.panel)) route.holds
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
  

    
let remove_hold hold_var =
  Lwd.set current_holds_var (
      List.filter (fun v -> Lwd.peek v <> Lwd.peek hold_var) @@ Lwd.peek current_holds_var
    )
