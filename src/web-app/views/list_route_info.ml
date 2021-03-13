open Model
open Tyxml_lwd.Html
open Lwd_infix

let make_var list_route_var =
  let$* list_route:Route.t list = Lwd.get list_route_var in
  let div_info = List.map Route_view.make list_route in
  (* let holds_info = List.map Hold_view.make list_route.holds in *)
  div ~a:[a_class (Lwd.pure ["right-panel-route"])] [
      div ~a:[] div_info;
      (* div ~a:[a_class (Lwd.pure(["route-hold-list"]))] holds_info; *)
    ]
  
let make list_route =
  let div_info = List.map Route_view.make list_route in
  (* let holds_info = List.map Hold_view.make list_route.holds in *)
  div ~a:[a_class (Lwd.pure ["all-route-info"])] [
      div ~a:[] div_info;
      (* div ~a:[a_class (Lwd.pure(["route-hold-list"]))] holds_info; *)
    ]
  
