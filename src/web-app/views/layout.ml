open Tyxml_lwd.Html
  
let make () =
  let open Logic in
  let panels_div  = Panel_view.make_panel_list_div all_panels_var current_panel_var in
  let main_panel_div  = Panel_view.make_main_panel_div current_panel_var in
  let routes_div  = Route_view.make_route_list_div all_routes_var in
  let route_info_div  = Route_view.make_current_route_div current_route_var in
  let panel_form = Panel_view.make_panel_form panel_form_var in
  
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
