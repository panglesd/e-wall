open Webapp_libs
open Lwt.Syntax
   
let (all_panels_var:Model.Panel.t list Lwd.var) = Lwd.var [] 
let current_route_var = Lwd.var @@ None 
let current_panel_var = Lwd.var @@ None 
let (panel_form_var:unit option Lwd.var) = Lwd.var @@ None
(* let current_route_var = Lwd.var @@ Some (Model.Route.make ~name:"" ~holds:[] ~feet:All ()) 
 * let current_panel_var = Lwd.var @@ Some (Model.Panel.make ~name:"" ~filename:"" ())  *)
let all_routes_var = Lwd.var [] 



let update_panel_list () =
  let+ all_panels = Request.get_all_panels () in
  print_endline("got all panels!");
  Lwd.set all_panels_var all_panels;
  Lwd.set current_panel_var (Some (List.hd all_panels))


  
let update_route_list () = 
  let+ all_routes = Request.get_all_routes () in
  Lwd.set all_routes_var all_routes;
  Lwd.set current_route_var (Some (List.hd all_routes))

