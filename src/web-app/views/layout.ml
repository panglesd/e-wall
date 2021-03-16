open Tyxml_lwd.Html
(* open Lwd.Infix *)
open Lwd_infix
open Webapp_libs
open Lwt.Syntax
   
let v = Lwd.var 1

let make_panels_div all_panels_var current_panel_var =
  let$* all_panels = Lwd.get all_panels_var in
  let on_click panel = Some (fun _ev ->
    Lwd.set current_panel_var (Some panel); false) in
  let l = List.map (Panel_view.div_from_panel ~on_click) all_panels in
  div ~a:[a_class (Lwd.pure ["bottom-panel"])] l

let make_panel_form panel_form_var =
  let$* class_lwd = Lwd.get panel_form_var in
  
  div ~a:[a_class (Lwd.pure ["panel-form"; match class_lwd with Some () -> "visible" | None -> "invisible"])] [
      form ~a:[a_action (Lwd.pure "panel"); a_method (Lwd.pure `Post); a_enctype (Lwd.pure "multipart/form-data")] [
          txt (Lwd.pure "Nom : ");
          input ~a:[a_input_type (Lwd.pure `Text); a_name (Lwd.pure "panel_name")] ();
          txt (Lwd.pure "Fichier : ");
          input ~a:[a_input_type (Lwd.pure `File); a_name (Lwd.pure "panel_file")] ();
          txt (Lwd.pure "Soumettre : ");
          input ~a:[a_input_type (Lwd.pure `Submit); a_name (Lwd.pure "new-panel")] ();
        ]
    ]
  
let make_main_panel_div current_panel_var =
  let$* current_panel_opt:Model.Panel.t option = Lwd.get current_panel_var in
  match current_panel_opt with
    None -> div ~a:[a_class (Lwd.pure ["main-panel"])] []
  | Some current_panel -> 
     div ~a:[a_class (Lwd.pure ["main-panel"])] [img ~src:(Lwd.pure ("img/panel-img/"^current_panel.filename)) ~alt:(Lwd.pure "current panel") ()]

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

  let all_panels_var = Lwd.var [] in
  let current_route_var = Lwd.var @@ None in
  let current_panel_var = Lwd.var @@ None in
  let panel_form_var = Lwd.var @@ None in
  (* let current_route_var = Lwd.var @@ Some (Model.Route.make ~name:"" ~holds:[] ~feet:All ()) in
   * let current_panel_var = Lwd.var @@ Some (Model.Panel.make ~name:"" ~filename:"" ()) in *)
  let all_routes_var = Lwd.var [] in

  let _ =
    let+ all_panels = Request.get_all_panels () in
    print_endline("got all panels!");
    Lwd.set all_panels_var all_panels;
    Lwd.set current_panel_var (Some (List.hd all_panels)) in

  let _ = 
    let+ all_routes = Request.get_all_routes () in
    Lwd.set all_routes_var all_routes;
    Lwd.set current_route_var (Some (List.hd all_routes)) in
  (* let texte_string = txt @@ Lwd.map ~f:string_of_int (Lwd.get v) in *)

  let panels_div  = make_panels_div all_panels_var current_panel_var in
  let main_panel_div  = make_main_panel_div current_panel_var in
  let routes_div  = make_routes_div all_routes_var in
  let route_info_div  = make_route_info_div current_route_var in
  let panel_form = make_panel_form panel_form_var in
  
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
