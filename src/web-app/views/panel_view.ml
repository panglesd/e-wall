open Tyxml_lwd.Html
open Model
open Lwd_infix
open Logic
(* open Lwt.Syntax *)
(* open Webapp_libs *)

   
   
(* A few functions to help testing *)
   
let i = ref 0
let _new_panel ():Panel.t = {
    id = string_of_int(!i);
    name = "panel numéro " ^ (string_of_int !i);
    filename = "filename.jpg"
  }
(* ********************************* *)
(* Creating a div of a panel         *)
(* ********************************* *)
                         
let div_of_panel ?on_click (panel:Panel.t) =
  let panelImg = img
                   ~src:(Lwd.pure @@ "img/panel-img/" ^ (panel.filename))
                   ~alt:(Lwd.pure panel.name)
                   () in
  let panelName = txt (Lwd.pure panel.name) in
  let on_click = Lwd.pure Opt_monad.(on_click *=< panel) in
    div ~a:[a_class (Lwd.pure ["panel"]); a_onclick on_click] [panelImg; panelName]

(* ********************************* *)
(* Creating a div of a panel list    *)
(* ********************************* *)

let div_list_from_panel_list ?f panel_list =
  List.map (div_of_panel ?on_click:f) panel_list

(* ********************************* *)
(* Creating the panel layouts        *)
(* ********************************* *)

(* The "list of panels" div          *)
  
let make_panel_list_div all_panels_var current_panel_var =
  let update_div =  input ~a:[a_value (Lwd.pure "Update");a_input_type (Lwd.pure `Button); a_onclick (Lwd.pure @@ Some (fun _ -> ignore @@ Logic.Main_logic.update_panel_list (); false))] () in
  let$* all_panels = Lwd.get all_panels_var in
  let on_click panel = Some (fun _ev ->
    Lwd.set current_panel_var (Some panel); false) in
  let l = List.map (div_of_panel ~on_click) all_panels in
  div ~a:[a_class (Lwd.pure ["bottom-panel"])] (update_div::l)

(* The "main panel" div              *)

(* let move_hold hold_var =
 *   let f = fun e ->
 *     let old_hold = Lwd.peek hold_var in
 *     let open Model.Hold in
 *     let hx,hy = old_hold.position in
 *     let elem = Js_of_ocaml.Dom.eventTarget e in
 *     let x,y = Js_of_ocaml.Dom_html.elementClientPosition elem in
 *     let x2,y2 = Js_of_ocaml.Dom_html.eventAbsolutePosition e in
 *     Printf.printf "old_hold pos %d %d\n" x y;
 *     Printf.printf "event absolute pos %d %d\n" x2 y2;
 *     let new_hold = Model.Hold.make
 *                      ~id:old_hold.id
 *                      ~panel:old_hold.panel
 *                      ~position:(hx+x-x2, hy+y-y2)
 *                      ~size:old_hold.size
 *                      ~name:old_hold.name
 *     in
 *     Lwd.set hold_var new_hold;
 *     false in
 *   Some f *)



  
let tool_div =
     let classes =
       let open Panel_logic in
       let$ tool = Lwd.get tool_var in
       match tool with
         Add -> (["selected";"tool"], ["tool"], ["tool"])
       | Move -> (["tool"], ["selected";"tool"], ["tool"])
       | Delete -> (["tool"], ["tool"], ["selected";"tool"])
     in
     let$* ui_state = Lwd.get Main_logic.ui_state_var in
     match ui_state with
       Main_logic.Editing_Panel ->
       div ~a:[a_class (Lwd.pure ["tools"])] [
           div ~a:[a_class (Lwd.Infix.(classes >|= fun (a,_,_) -> a));
                   a_onclick (Lwd.pure @@ Some(fun _e -> Lwd.set Panel_logic.tool_var Panel_logic.Add; false))]
             [txt (Lwd.pure "Add")];
           div ~a:[a_class (Lwd.Infix.(classes >|= fun (_,a,_) -> a));
                   a_onclick (Lwd.pure @@ Some(fun _e -> Lwd.set Panel_logic.tool_var Panel_logic.Move; false))]
             [txt (Lwd.pure "Move")];
           div ~a:[a_class (Lwd.Infix.(classes >|= fun (_,_,a) -> a));
                   a_onclick (Lwd.pure @@ Some(fun _e -> Lwd.set Panel_logic.tool_var Panel_logic.Delete; false))]
             [txt (Lwd.pure "Delete")];
           input ~a:[a_input_type (Lwd.pure `Button); a_value (Lwd.pure "Valider les modifications");
                     a_onclick (
                         let$ f = Panel_logic.save_panel in
                         Some (fun e -> ignore @@ f e;
                                        Lwd.set Main_logic.ui_state_var Main_logic.Viewing_Content;
                                        false))
             ] ();
         ]
     | _ ->
        div ~a:[a_class (Lwd.pure ["tools"])] [
            input ~a:[a_value (Lwd.pure "Modifier le panel");
                      a_input_type (Lwd.pure `Button);
                      a_onclick (Lwd.pure @@ Some (fun _e ->
                                                 Main_logic.(Lwd.set ui_state_var Editing_Panel);
                                                 false))] ()
          ]


  
    

  
let make_main_panel_div current_panel_var current_holds_var =
  let$* current_panel_opt = Lwd.get current_panel_var in
  match current_panel_opt with
    None -> div ~a:[a_class (Lwd.pure ["main-panel"])] []
  | Some (current_panel) ->
     let$* holds_div_list =
       (* let$* loaded = Lwd.get loaded in
        * if not loaded then
        *   Lwd.pure []
        * else *)
         let$ current_holds = Lwd.get current_holds_var in
         List.map Hold_view.hold_in_panel_div (List.filter (fun hold -> Model.Hold.((Lwd.peek hold).panel) = current_panel) current_holds) in
     (* let on_click = add_hold_on_panel_callback current_holds_var current_panel current_holds in *)
     let click_callback = Main_logic.make_callback
                            ~editing_panel:Panel_logic.mouse_click_callback
                            ~editing_route:Panel_logic.mouse_click_callback
                            ~viewing_content:(fun _var -> Lwd.pure None) None
     and mousemove_callback = Main_logic.make_callback
                                ~editing_panel:Panel_logic.mouse_move_callback
                                ~editing_route:Panel_logic.mouse_move_callback
                                ~viewing_content:(fun _var -> Lwd.pure None) None
     and mouseup_callback = Main_logic.make_callback
                              ~editing_panel:Panel_logic.mouse_up_callback
                              ~editing_route:Panel_logic.mouse_up_callback
                              ~viewing_content:(fun _var -> Lwd.pure None) None
     and mousedown_callback = Main_logic.make_callback
                                ~editing_panel:Panel_logic.mouse_down_callback
                                ~editing_route:Panel_logic.mouse_down_callback
                                ~viewing_content:(fun _var -> Lwd.pure None) None in
     let size =
       let$ loaded = Lwd.get Main_logic.loaded in
       if loaded then
         let img = Webapp_libs.Utils.get_img () in
         print_endline @@ "width:" ^ string_of_int(img##.width) ^ "px";
         "width:" ^ string_of_int(img##.width) ^ "px"
       else
         "width: 0px" in
     div
       ~a:[a_class (Lwd.pure ["main-panel"])]
       [
         (* input ~a:[a_input_type (Lwd.pure `Button); a_onclick (Lwd.pure @@ Some (fun _ -> ignore @@ Logic.update_panel_list (); false))] (); *)
         div ~a:[a_class (Lwd.pure ["panel-hold-container"]) (* ; a_onclick (Lwd.pure @@ Some on_click) *)] 
           ([
               div ~a:[a_style size; a_class (Lwd.pure ["panel-hold-container-2"])] ([
                   img ~a:[
                       a_id (Lwd.pure "main-panel-img");
                       a_onclick click_callback;
                       a_onmouseup mouseup_callback;
                       a_onmousemove mousemove_callback;
                       a_onmousemove mousedown_callback;
                       a_onload (Lwd.pure @@ Some (fun _e -> Lwd.set Main_logic.loaded true; false))
                     ]
                     ~src:(Lwd.pure ("img/panel-img/"^Model.Panel.(current_panel.filename)))
                     ~alt:(Lwd.pure "current panel") ();
                 ] @ holds_div_list)
             ] );
         tool_div
       ]
    
(* The "add a panel" form            *)

let make_panel_form ()(* panel_form_var *) =
  (* let$* panel_form = Lwd.get panel_form_var in *)
  div ~a:[a_class (Lwd.pure ["panel-form"(* ; match panel_form with Some () -> "visible" | None -> "invisible" *)])] [
      form ~a:[a_action (Lwd.pure "panel"); a_method (Lwd.pure `Post); a_enctype (Lwd.pure "multipart/form-data")] [
          txt (Lwd.pure "Nom : ");
          input ~a:[a_input_type (Lwd.pure `Text); a_name (Lwd.pure "panel_name")] ();
          txt (Lwd.pure "Fichier : ");
          input ~a:[a_input_type (Lwd.pure `File); a_name (Lwd.pure "panel_file")] ();
          txt (Lwd.pure "Soumettre : ");
          input ~a:[a_input_type (Lwd.pure `Submit); a_name (Lwd.pure "new-panel")] ();
        ]
    ]
  
