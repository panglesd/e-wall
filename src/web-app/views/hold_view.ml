open Model
open Tyxml_lwd.Html
open Lwd_infix
open Logic

let make (hold_var:Hold.t Lwd.var) =
  let$* hold = Lwd.get hold_var in
  let$* ui_state = Lwd.get Main_logic.ui_state_var in
  match ui_state with
  |  Main_logic.Editing_Panel ->
      let _panel_string = hold.panel.name in
      let (x,y) = hold.position |> fun (x,y) -> (int_of_float x, int_of_float y) in
      div ~a:[a_class (Lwd.pure ["hold-info"])] [
          div ~a:[a_class (Lwd.pure ["round-hold-container"])] [
              img ~a:[a_style (Lwd.pure @@ Printf.sprintf "position:relative; left:%dpx; top: %dpx" (20-x) (20-y))] ~src:(Lwd.pure @@ "img/panel-img/" ^ (hold.panel.filename)) ~alt:(Lwd.pure "alt")()
            ];
         (* div ~a:[a_class (Lwd.pure ["hold-panel-name"])] [txt (Lwd.pure panel_string)]; *)
          input ~a:[
              a_class (Lwd.pure ["hold-name"]);
              a_input_type (Lwd.pure `Text);
              a_value (Lwd.pure hold.name);
              a_onchange (Lwd.pure @@
                            Some (fun e ->
                                let target_js_opt = e##.currentTarget in
                                let target_opt = Js_of_ocaml.Js.Opt.to_option target_js_opt in
                                match target_opt with
                                  None -> false
                                | Some target ->
                                   match  Js_of_ocaml.Js.Opt.to_option @@ Js_of_ocaml.Dom_html.CoerceTo.input target  with
                                     None -> false
                                   | Some input_target ->
                                      let input_value = Js_of_ocaml.Js.to_string input_target##.value in
                                      Lwd.set hold_var (Model.Hold.set_name hold input_value);
                                      false
                              )
                )
            ] ()
        ] 
  | Main_logic.Editing_Route 
  | Main_logic.Viewing_Route_List
  | Main_logic.Viewing_Route  ->
     let panel_string = hold.panel.name in 
     div ~a:[a_class (Lwd.pure ["hold-info"])] [
         img ~src:(Lwd.pure "rien") ~alt:(Lwd.pure "alt")();
         div ~a:[a_class (Lwd.pure ["hold-panel-name"])] [txt (Lwd.pure panel_string)];
         div ~a:[a_class (Lwd.pure ["hold-name"])] [txt (Lwd.pure hold.name)];
       ] 


let hold_in_panel_div (hold_var:Model.Hold.t Lwd.var) =
  let$* hold = Lwd.get hold_var in
  let$* loaded = Lwd.get Main_logic.loaded in
  let click_callback = Main_logic.make_callback
                         ~editing_panel:Panel_logic.mouse_click_callback
                         ~editing_route:Route_logic.mouse_click_callback
                         ~viewing_route_list:(fun _var -> Lwd.pure None)
                         ~viewing_route:(fun _var -> Lwd.pure None) (Some hold_var)
  and mousemove_callback = Main_logic.make_callback
                             ~editing_panel:Panel_logic.mouse_move_callback
                             ~editing_route:(fun _var -> Lwd.pure None)
                             ~viewing_route:(fun _var -> Lwd.pure None)
                             ~viewing_route_list:(fun _var -> Lwd.pure None) (Some hold_var)
  and mouseup_callback = Main_logic.make_callback
                           ~editing_panel:Panel_logic.mouse_up_callback
                           ~editing_route:Panel_logic.mouse_click_callback
                           ~viewing_route:(fun _var -> Lwd.pure None)
                           ~viewing_route_list:(fun _var -> Lwd.pure None) (Some hold_var)
  and mousedown_callback = Main_logic.make_callback
                             ~editing_panel:Panel_logic.mouse_down_callback
                             ~editing_route:(fun _var -> Lwd.pure None)
                             ~viewing_route:(fun _var -> Lwd.pure None)
                             ~viewing_route_list:(fun _var -> Lwd.pure None) (Some hold_var) in
  let (x0,y0) = hold.position in
  Printf.printf "x0, y0 are now %f %f\n" x0 y0;
  let img = Webapp_libs.Utils.get_img () in
  let ready,naturalHeight, naturalWidth = match Js_of_ocaml.Js.Optdef.(
      to_option img##.naturalHeight, to_option img##.naturalWidth) with
    | Some n1, Some n2 -> true, n1, n2
    | _ -> false,1,1 in
  let y = y0*.float img##.height /. float naturalHeight
  and x = x0*.float img##.width /. float naturalWidth in
  div ~a:[
      a_style (Lwd.pure @@ Printf.sprintf "left:%dpx; top: %dpx" (int_of_float x-10) (int_of_float y-10));
      a_class (Lwd.pure @@ if loaded && ready then ["hold"] else []);
      (* a_onclick (Lwd.pure @@ move_hold hold_var); *)
      a_onmousedown mousedown_callback;
      a_onclick click_callback;
      a_onmousemove mousemove_callback;
      a_onmouseup mouseup_callback;
    ] []
    

let make_hold_list_div =
  let$* ui_state = Lwd.get Main_logic.ui_state_var
  and$ current_holds = Lwd.get Main_logic.current_holds_var in
  match ui_state with
    Main_logic.Editing_Panel ->
    let holds_info = List.map make @@ current_holds in
    div ~a:[a_class (Lwd.pure ["right-panel-holds"])] holds_info
  | Main_logic.Editing_Route
    | Main_logic.Viewing_Route
    | Main_logic.Viewing_Route_List -> div ~a:[] []
