open Model
open Tyxml_lwd.Html
open Lwd_infix
open Logic
(* ********************************* *)
(* Creating a div of a route         *)
(* ********************************* *)

let div_of_route ?on_click (route:Route.t) = 
  let cotation_string = match route.cotation with
      None -> "Non côtée"
    | Some cot -> Cotation.string_of_cotation cot in
  let feet_string, classList = match route.feet with
      All -> "Tout pied autorisés", "all-feet"
    | Only -> "Seul les prises de main peuvent être utilisées avec les pieds", "restricted-feet" in
  let name =
    div ~a:[a_class (Lwd.pure ["route-name"])] [txt (Lwd.pure route.name)]
  in
  let on_click = Lwd.pure Opt_monad.(on_click *=< route) in
  div ~a:[a_class (Lwd.pure ["route-info"; classList]); a_onclick on_click] [
      (* div ~a:[a_class (Lwd.pure ["route-name"])] [txt (Lwd.pure route.name)]; *)
      name;
      div ~a:[a_class (Lwd.pure ["route-cotation"])] [txt (Lwd.pure cotation_string)];
      div ~a:[a_class (Lwd.pure ["route-feet"])] [txt (Lwd.pure feet_string)];
    ] 

let div_of_route_detailed ?on_click (route:Route.t) = 
  let cotation_string = match route.cotation with
      None -> "Non côtée"
    | Some cot -> Cotation.string_of_cotation cot in
  let feet_string, classList = match route.feet with
      All -> "Tout pied autorisés", "all-feet"
    | Only -> "Seul les prises de main peuvent être utilisées avec les pieds", "restricted-feet" in
  let name =
    let$* ui_state = Lwd.get Main_logic.ui_state_var in
    match ui_state with
      Main_logic.Editing_Route -> 
       input ~a:[a_class (Lwd.pure ["route-name"]);
                 a_input_type (Lwd.pure `Text);
                 a_placeholder (Lwd.pure "Nom de la voie");
                 a_onchange (Route_logic.onchange_name_callback);
                 a_value((Lwd.pure route.name))] ()
    | _ -> 
       div ~a:[a_class (Lwd.pure ["route-name"])] [txt (Lwd.pure route.name)]
  in
  let on_click = Lwd.pure Opt_monad.(on_click *=< route) in
  div ~a:[a_class (Lwd.pure ["route-info"; classList]); a_onclick on_click] [
      (* div ~a:[a_class (Lwd.pure ["route-name"])] [txt (Lwd.pure route.name)]; *)
      name;
      div ~a:[a_class (Lwd.pure ["route-cotation"])] [txt (Lwd.pure cotation_string)];
      div ~a:[a_class (Lwd.pure ["route-feet"])] [txt (Lwd.pure feet_string)];
    ] 

(* ********************************* *)
(* Creating a div of a route list    *)
(* ********************************* *)

let div_list_from_route_list ?f route_list =
  List.map (div_of_route ?on_click:f) route_list

(* ********************************* *)
(* Creating the route layouts        *)
(* ********************************* *)
  
(* The "list of routes" div          *)

let make_route_list_div =
  let$* ui_state = Lwd.get Main_logic.ui_state_var in
  match ui_state with
    Main_logic.Editing_Panel | Main_logic.Editing_Route -> div ~a:[] []
  | Main_logic.Viewing_Route_List | Main_logic.Viewing_Route ->
  let$* list_route = Lwd.get Main_logic.all_routes_var in
  let div_info = div_list_from_route_list ~f:Route_logic.select_route_callback list_route in
  div ~a:[a_class (Lwd.pure ["all-route-info"])] [
      div ~a:[] div_info;
      div ~a:[] [
          input ~a:[
              a_input_type (Lwd.pure `Button);
              a_value (Lwd.pure "Ajouter une voie");
              a_onclick Route_logic.create_route_callback
            ] ()
        ]
    ]
    
(* The "current route" div           *)

let make_current_route_div =
  let close_div = input ~a:[a_input_type (Lwd.pure `Button);
                            a_value (Lwd.pure "Retour à la liste des voies");
                            a_onclick Route_logic.close_route_callback] () in
  let$* current_route = Lwd.get Main_logic.current_route_var in
  match current_route with
    None -> 
     div ~a:[a_class (Lwd.pure ["right-panel-route"; "invisible"])] []
  | Some current_route ->
    let div_info = div_of_route_detailed current_route in
    let holds_info = List.map Hold_view.make (current_route.holds |> List.map Lwd.var) in
  div ~a:[a_class (Lwd.pure ["right-panel-route"])] [
      close_div;
      div_info;
      div ~a:[a_class (Lwd.pure(["route-hold-list"]))] (holds_info @ [txt (Lwd.pure "Cliquer sur une prise pour la rajouter au parcours")]);
      input ~a:[a_input_type (Lwd.pure `Button);
                a_value (Lwd.pure "Enregistrer la voie");
                a_onclick Route_logic.save_route] () 
    ]
