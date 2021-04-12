open Model
open Tyxml_lwd.Html
open Lwd_infix
open Logic
   
let make (hold:Hold.t) = 
  let panel_string = hold.panel.name in 
  div ~a:[a_class (Lwd.pure ["hold-info"])] [
      img ~src:(Lwd.pure "rien") ~alt:(Lwd.pure "alt")();
      div ~a:[a_class (Lwd.pure ["hold-panel-name"])] [txt (Lwd.pure panel_string)];
      div ~a:[a_class (Lwd.pure ["hold-name"])] [txt (Lwd.pure hold.name)];
    ] 


let hold_in_panel_div (hold_var:Model.Hold.t Lwd.var) =
  let$* hold = Lwd.get hold_var in
  let$* _loaded = Lwd.get Main_logic.loaded in
  let click_callback = Main_logic.make_callback ~editing_panel:Panel_logic.mouse_click_callback
                         ~editing_route:Route_logic.mouse_click_callback
                         ~viewing_content:(fun _var -> Lwd.pure None) (Some hold_var)
  and mousemove_callback = Main_logic.make_callback ~editing_panel:Panel_logic.mouse_move_callback
                             ~editing_route:Panel_logic.mouse_click_callback
                             ~viewing_content:(fun _var -> Lwd.pure None) (Some hold_var)
  and mouseup_callback = Main_logic.make_callback ~editing_panel:Panel_logic.mouse_up_callback
                           ~editing_route:Panel_logic.mouse_click_callback
                           ~viewing_content:(fun _var -> Lwd.pure None) (Some hold_var)
  and mousedown_callback = Main_logic.make_callback ~editing_panel:Panel_logic.mouse_down_callback
                             ~editing_route:Panel_logic.mouse_click_callback
                             ~viewing_content:(fun _var -> Lwd.pure None) (Some hold_var) in
  let (x0,y0) = hold.position in
  Printf.printf "x0, y0 are now %d %d\n" x0 y0;
  let img = match Js_of_ocaml.Dom_html.(getElementById_coerce "main-panel-img" CoerceTo.img) with
      None -> failwith "No main-panel-img"
    | Some elem -> elem in
  let naturalHeight, naturalWidth = match Js_of_ocaml.Js.Optdef.(
      to_option img##.naturalHeight,
      to_option img##.naturalWidth) with
    | Some n1, Some n2 -> n1, n2
    | _ -> 1,1 in
  let y = y0*img##.height/naturalHeight
  and x = x0*img##.width/naturalWidth in
  div ~a:[
      a_style (Lwd.pure @@ Printf.sprintf "left:%dpx; top: %dpx" (x-10+0*img##.offsetLeft) (y-10));
      a_class (Lwd.pure [hold.name; "hold"]);
      (* a_onclick (Lwd.pure @@ move_hold hold_var); *)
      a_onmousedown mousedown_callback;
      a_onclick click_callback;
      a_onmousemove mousemove_callback;
      a_onmouseup mouseup_callback;
    ] []
    
