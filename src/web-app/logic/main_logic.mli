(* Getters *)
(* Atomic getters (maybe to remove?) *)
val get_current_panel : Model.Panel.t option Lwd.t
val get_all_panels : Model.Panel.t list Lwd.t
val get_all_routes : Model.Route.t list Lwd.t
val get_current_route : Model.Route.t option Lwd.t
val get_current_holds : Model.Hold.t Lwd.var list Lwd.t
val get_focused_hold : Model.Hold.t option Lwd.t

val get_current_panel_val : unit -> Model.Panel.t option
val get_all_panels_val : unit -> Model.Panel.t list
val get_all_routes_val : unit -> Model.Route.t list
val get_current_route_val : unit -> Model.Route.t option
val get_current_holds_val : unit -> Model.Hold.t Lwd.var list
val get_focused_hold_val : unit -> Model.Hold.t option
(* Elaborate getters *)
val get_panels_to_show : Model.Panel.t list Lwd.t

  
(* Setters *)
(* Should we be able to set panel_list, route_list and current_hold? *)
val set_current_panel : Model.Panel.t -> unit
val set_panel_list : Model.Panel.t list -> unit
val set_route_list : Model.Route.t list -> unit
val set_current_route : Model.Route.t option -> unit
val set_current_holds : Model.Hold.t Lwd.var list -> unit
val add_hold : Model.Hold.t -> unit
val set_focused_hold : Model.Hold.t option -> unit  
(* Receive from server *)
val update_current_holds : unit -> unit Lwt.t
val update_panel_list : unit -> unit Lwt.t
val update_route_list : unit -> unit Lwt.t

(* Send to server *)
val save_current_holds : Model.Hold.t_list -> unit Lwt.t
val remove_hold : Model.Hold.t Lwd.var -> unit

