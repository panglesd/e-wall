type ui_state =
  Editing_Panel
| Editing_Route
| Viewing_Route_List
| Viewing_Route

val get_ui_state : ui_state Lwd.t
val get_ui_state_val : ui_state
val set_ui_state : ui_state -> unit

val loaded : bool Lwd.var

(* Callback utility *)
val make_callback : editing_panel:('a -> 'b Lwd.t) -> editing_route:('a -> 'b Lwd.t) -> viewing_route:('a -> 'b Lwd.t) -> viewing_route_list:('a -> 'b Lwd.t) -> 'a -> 'b Lwd.t
