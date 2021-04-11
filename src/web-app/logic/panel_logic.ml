(* open Tyxml_lwd.Html *)
(* open Model *)
open Lwd_infix

type tool = Add | Move | Delete
let tool_var = Lwd.var Move
let focused_var = Lwd.var None

let mouse_click_add : Tyxml_lwd.Xml.mouse_event_handler =
  let f = fun e ->
    let elem = Js_of_ocaml.Dom.eventTarget e in
    let target = e##.target in
    let current_target = e##.currentTarget in
    if target = current_target then
      (* let relTarget = Js_of_ocaml.Js.Opt.to_option @@ Js_of_ocaml.Dom_html.eventRelatedTarget e in *)
      let x,y = Js_of_ocaml.Dom_html.elementClientPosition elem in
      let x2,y2 = Js_of_ocaml.Dom_html.eventAbsolutePosition e in
      match (Lwd.peek Main_logic.current_panel_var) with
        Some panel ->
         let new_hold = Model.Hold.make_rand_id ~panel ~position:(x2-x,y2-y) ~size:10 ~name:"10" in
         Lwd.set (Main_logic.current_holds_var) ((Lwd.var new_hold)::(Lwd.peek Main_logic.current_holds_var));
         false
      | None -> false  
    else begin
        (* Printf.printf "bliblibli\n"; *)
        false end in
  Lwd.pure @@ Some f

let mouse_click_remove hold_var =
  let f = fun _e ->
    Main_logic.remove_hold hold_var;
    false
  in
  Lwd.pure @@ Some f

let mouse_click_callback hold_var_opt =
  let$* tool = Lwd.get tool_var in
  match tool with
    Move -> Lwd.pure None
  | Add -> mouse_click_add
  | Delete -> match hold_var_opt with None -> Lwd.pure None | Some hold_var ->  mouse_click_remove hold_var
                                                                              
                                                                              
let mouse_move_move : Tyxml_lwd.Xml.mouse_event_handler =
  let$ focused = Lwd.get focused_var in
  match focused with
    None -> 
     None
  | Some ((old_hold_var : Model.Hold.t Lwd.var),(x,y),(mx, my)) ->
     Some (fun e ->
         let old_hold = Lwd.peek old_hold_var in
         let nmx, nmy = Js_of_ocaml.Dom_html.eventAbsolutePosition e in
         let new_hold = Model.Hold.make
                          ~id:old_hold.id
                          ~panel:old_hold.panel
                          ~position:(x+(nmx-mx), y+(nmy-my))
                          ~size:old_hold.size
                          ~name:old_hold.name
         in
         Lwd.set old_hold_var new_hold;
         false
       )

let mouse_move_callback =
  let$* tool = Lwd.get tool_var in
  match tool with
    Move -> mouse_move_move
  | Add -> Lwd.pure None
  | Delete -> Lwd.pure None

let mouse_up_move : Tyxml_lwd.Xml.mouse_event_handler =
  let$ focused = Lwd.get focused_var in
  match focused with
    None -> None
  | Some _ ->
     Some (fun _e -> Lwd.set focused_var None; false)

let mouse_up_callback = 
  let$* tool = Lwd.get tool_var in
  match tool with
    Move -> mouse_up_move
  | Add -> Lwd.pure None
  | Delete -> Lwd.pure None

let mouse_down_move hold_var =
  Lwd.pure @@
    Some (fun e ->
        let xev,yev = Js_of_ocaml.Dom_html.eventAbsolutePosition e in
        let x,y = Model.Hold.((Lwd.peek hold_var).position) in
        Lwd.set focused_var (Some (hold_var, (x, y), (xev,yev))); false)
  
let mouse_down_callback hold_var = 
  let$* tool = Lwd.get tool_var in
  match tool with
    Move -> mouse_down_move hold_var
  | Add -> Lwd.pure None
  | Delete -> Lwd.pure None

let mouse_leave_move hold_var =
  Lwd.pure @@
    Some (fun e ->
        let xev,yev = Js_of_ocaml.Dom_html.eventAbsolutePosition e in
        let x,y = Model.Hold.((Lwd.peek hold_var).position) in
        Lwd.set focused_var (Some (hold_var, (x, y), (xev,yev))); false)
  
let mouse_leave_callback hold_var = 
  let$* tool = Lwd.get tool_var in
  match tool with
    Move -> mouse_leave_move hold_var
  | Add -> Lwd.pure None
  | Delete -> Lwd.pure None

                       (* let hold_mouseleave_callback = mouse_up_callback *)
                       (* let$ focused = Lwd.get focused_var in
                        * match focused with
                        *   None -> None
                        * | Some _ -> 
                        *    Some (fun _e -> Lwd.set focused_var None; false) *)
                
                