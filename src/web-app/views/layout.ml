open Tyxml_lwd.Html
(* open Lwd.Infix *)
open Lwd_infix
open Webapp_libs
open Lwt.Syntax
   
let make_routes_div all_routes_var =
  let$* all_routes = (Lwd.get all_routes_var) in
  List_route_info.make all_routes
  (* div ~a:[a_class (Lwd.pure ["bottom-panel"])] l *)

let make_route_info_div current_route_var = 
  let$* current_route_opt:Model.Route.t option = Lwd.get current_route_var in
  match current_route_opt with
    None -> div ~a:[a_class (Lwd.pure ["route-info"])] []
  | Some current_route -> Current_route_info.make current_route
  
let make () =


  (* let texte_string = txt @@ Lwd.map ~f:string_of_int (Lwd.get v) in *)

  let panels_div  = Panel_view.make_panels_div all_panels_var current_panel_var in
  let main_panel_div  = Panel_view.make_main_panel_div current_panel_var in
  let routes_div  = make_routes_div all_routes_var in
  let route_info_div  = make_route_info_div current_route_var in
  let panel_form = Panel_view.make_panel_form panel_form_var in
  
  (* let texte_string = Lwd.get v
   *                    >|= string_of_int
   *                    >|= (^) "La valeur est : " in
   * let _texte = txt texte_string in
   * let _f _ = Lwd.set v (Lwd.peek v + 1) ; false in *)

  (* div ~a:[a_class (Lwd.pure ["bli"]); a_onclick (Lwd.pure (Some f))] [ texte ;
   *                                                                      Panel_view.panel_div]; *)

  div ~a:[a_class (Lwd.pure ["app-root"])] [
      div ~a:[a_class (Lwd.pure ["panels-root"])] [
          main_panel_div;
          panels_div ;
          panel_form
        ];
      div ~a:[a_class (Lwd.pure ["routes-root"])] [
          routes_div;
          route_info_div
        ];
    ]
