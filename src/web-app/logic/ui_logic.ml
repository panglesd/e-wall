open Lwd_infix

(** Ui state Variables  *)

type ui_state =
  Editing_Panel
| Editing_Route
| Viewing_Route_List
| Viewing_Route

let ui_state_var = Lwd.var Viewing_Route_List
let loaded = Lwd.var false

let get_ui_state = Lwd.get ui_state_var
let get_ui_state_val = Lwd.peek ui_state_var
           
let set_ui_state state = match state with
    Editing_Panel -> Lwd.set ui_state_var Editing_Panel
  | Editing_Route -> Lwd.set ui_state_var Editing_Route
  | Viewing_Route_List -> Lwd.set ui_state_var Viewing_Route_List
  | Viewing_Route -> Lwd.set ui_state_var Viewing_Route

let make_callback ~editing_panel ~editing_route ~viewing_route ~viewing_route_list hold_var_opt =
  let$* ui_state = get_ui_state in
  match ui_state with
    Editing_Panel -> editing_panel hold_var_opt
  | Editing_Route -> editing_route hold_var_opt
  | Viewing_Route_List -> viewing_route_list hold_var_opt
  | Viewing_Route -> viewing_route hold_var_opt

