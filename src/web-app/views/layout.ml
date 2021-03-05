open Tyxml_lwd.Html
open Lwd.Infix
open Lwd_infix
open Webapp_libs
open Lwt.Syntax
   
let v = Lwd.var 1

let make_panels_div all_panels_var =
  let$* all_panels = Lwd.get all_panels_var in
  let l = List.map Panel_view.div_from_panel all_panels in
  div ~a:[a_class (Lwd.pure ["bottom-panel"])] l

let make_main_panel_div current_panel_var =
  let$* current_panel:Model.Panel.t = Lwd.get current_panel_var in
  div ~a:[a_class (Lwd.pure ["main-panel"])] [img ~src:(Lwd.pure ("img/panel/"^current_panel.filename)) ~alt:(Lwd.pure "current panel") ()]

let make_routes_div all_routes_var =
  let$* all_routes = (Lwd.get all_routes_var) in
  List_route_info.make all_routes
  (* div ~a:[a_class (Lwd.pure ["bottom-panel"])] l *)
  
let make () =

  let all_panels_var = Lwd.var [] in
  let current_route_var = Lwd.var @@ Model.Route.make ~name:"" ~holds:[] ~feet:All () in
  let current_panel_var = Lwd.var @@ Model.Panel.make ~name:"" ~filename:"" () in
  let all_routes_var = Lwd.var [] in

  let _ =
    let+ all_panels = Request.get_all_panels () in
    Lwd.set all_panels_var all_panels;
    Lwd.set current_panel_var (List.hd all_panels) in

  let _ = 
    let+ all_routes = Request.get_all_routes () in
    Lwd.set all_routes_var all_routes;
    Lwd.set current_route_var (List.hd all_routes) in
  (* let texte_string = txt @@ Lwd.map ~f:string_of_int (Lwd.get v) in *)

  let panels_div  = make_panels_div all_panels_var in
  let main_panel_div  = make_main_panel_div current_panel_var in
  let routes_div  = make_routes_div all_routes_var in
  
  let texte_string = Lwd.get v
                     >|= string_of_int
                     >|= (^) "La valeur est : " in
  let texte = txt texte_string in
  let f _ = Lwd.set v (Lwd.peek v + 1) ; false in

  (* div ~a:[a_class (Lwd.pure ["bli"]); a_onclick (Lwd.pure (Some f))] [ texte ;
   *                                                                      Panel_view.panel_div]; *)

  div ~a:[a_class (Lwd.pure ["bli"])] [
      panels_div ;
      main_panel_div;
      routes_div
    ]
