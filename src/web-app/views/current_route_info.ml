open Model
open Tyxml_lwd.Html
open Lwd_infix

let make current_route =
  let div_info = Route_view.make current_route in
  let holds_info = List.map Hold_view.make current_route.holds in
  div ~a:[a_class (Lwd.pure ["right-panel-route"])] [
      div_info;
      div ~a:[a_class (Lwd.pure(["route-hold-list"]))] holds_info;
    ]
  
let make_var current_route_var =
  let$* current_route:Route.t = Lwd.get current_route_var in
  let div_info = Route_view.make current_route in
  let holds_info = List.map Hold_view.make current_route.holds in
  div ~a:[a_class (Lwd.pure ["right-panel-route"])] [
      div_info;
      div ~a:[a_class (Lwd.pure(["route-hold-list"]))] holds_info;
    ]
  
