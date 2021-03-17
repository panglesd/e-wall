open Model
open Tyxml_lwd.Html
open Lwd_infix

let div_of_route (route:Route.t) = 
  let cotation_string = match route.cotation with
      None -> "Non côtée"
    | Some cot -> Cotation.string_of_cotation cot in
  let feet_string, classList = match route.feet with
      All -> "Tout pied autorisés", "all-feet"
    | Only -> "Seul les prises de main peuvent être utilisées avec les pieds", "restricted-feet" in
  div ~a:[a_class (Lwd.pure ["route-info"; classList])] [
      div ~a:[a_class (Lwd.pure ["route-name"])] [txt (Lwd.pure route.name)];
      div ~a:[a_class (Lwd.pure ["route-cotation"])] [txt (Lwd.pure cotation_string)];
      div ~a:[a_class (Lwd.pure ["route-feet"])] [txt (Lwd.pure feet_string)];
    ] 

let make_var list_route_var =
  let$* list_route:Route.t list = Lwd.get list_route_var in
  let div_info = List.map Route_view.make list_route in
  div ~a:[a_class (Lwd.pure ["right-panel-route"])] [
      div ~a:[] div_info;
    ]
  
let make list_route =
  let div_info = List.map Route_view.make list_route in
  div ~a:[a_class (Lwd.pure ["all-route-info"])] [
      div ~a:[] div_info;
    ]
    
