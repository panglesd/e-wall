open Model
open Tyxml_lwd.Html
open Lwd_infix
  
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
  let on_click = Lwd.pure Opt_monad.(on_click *=< route) in
  div ~a:[a_class (Lwd.pure ["route-info"; classList]); a_onclick on_click] [
      div ~a:[a_class (Lwd.pure ["route-name"])] [txt (Lwd.pure route.name)];
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

let make_route_list_div list_route_var =
  let$* list_route = Lwd.get list_route_var in
  let div_info = div_list_from_route_list ~f:(fun route  -> Some (fun _ -> Logic.set_current_route (Some route);false)) list_route in
  div ~a:[a_class (Lwd.pure ["all-route-info"])] [
      div ~a:[] div_info;
    ]
    
(* The "current route" div           *)

let make_current_route_div current_route_var =
  let close_div = input ~a:[a_input_type (Lwd.pure `Button); a_onclick (Lwd.pure @@ Some (fun _ -> Logic.set_current_route None; false))] () in
  let$* current_route = Lwd.get current_route_var in
  match current_route with
    None -> 
     div ~a:[a_class (Lwd.pure ["right-panel-route"; "invisible"])] []
  | Some current_route ->
    let div_info = div_of_route current_route in
  let holds_info = List.map Hold_view.make current_route.holds in
  div ~a:[a_class (Lwd.pure ["right-panel-route"])] [
      close_div;
      div_info;
      div ~a:[a_class (Lwd.pure(["route-hold-list"]))] holds_info;
    ]

(* The "add a route" form            *)

let make_route_form route_form_var =
  let$* route_form = Lwd.get route_form_var in
  (* TODO ! *)
  div ~a:[a_class (Lwd.pure ["route-form"; match route_form with Some () -> "visible" | None -> "invisible"])] [
      form ~a:[a_action (Lwd.pure "route"); a_method (Lwd.pure `Post); a_enctype (Lwd.pure "multipart/form-data")] [
          txt (Lwd.pure "Nom : ");
          input ~a:[a_input_type (Lwd.pure `Text); a_name (Lwd.pure "route_name")] ();
          txt (Lwd.pure "Fichier : ");
          input ~a:[a_input_type (Lwd.pure `File); a_name (Lwd.pure "route_file")] ();
          txt (Lwd.pure "Soumettre : ");
          input ~a:[a_input_type (Lwd.pure `Submit); a_name (Lwd.pure "new-route")] ();
        ]
    ]
