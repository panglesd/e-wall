open Tyxml_lwd.Html
open Lwt.Syntax

let make () =
  let open Logic.Main_logic in
  let panels_div  = Panel_view.make_panel_list_div all_panels_var current_panel_var in
  let main_panel_div  = Panel_view.make_main_panel_div current_panel_var current_holds_var in
  let routes_div  = Route_view.make_route_list_div in
  let route_info_div  = Route_view.make_current_route_div in
  let hold_list_div  = Hold_view.make_hold_list_div in
  (* let panel_form = Panel_view.make_panel_form () (\* panel_form_var *\) in *)
  let _ = let* _ = update_panel_list () in
           let* _ = update_route_list () in
           let+ _ = update_current_holds () in
           ()
  in
  
  div ~a:[a_class (Lwd.pure ["app-root"])] [
      div ~a:[a_class (Lwd.pure ["panels-root"])] [
          main_panel_div;
          panels_div ;
          (* panel_form *)
        ];
      div ~a:[a_class (Lwd.pure ["routes-root"])] [
          routes_div;
          route_info_div;
          hold_list_div
        ];
    ]
