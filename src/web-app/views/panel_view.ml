open Tyxml_lwd.Html
open Model
open Lwd_infix
(* open Lwt.Syntax *)
(* open Webapp_libs *)

(* A few functions to help testing *)
   
let i = ref 0
let new_panel ():Panel.t = {
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
  let open Opt_monad in
    div ~a:[a_class (Lwd.pure ["panel"]); a_onclick (Lwd.pure (on_click *=< panel))] [panelImg; panelName]

(* ********************************* *)
(* Creating a div of a panel list    *)
(* ********************************* *)

let div_list_from_panel_list ?f panel_list =
  List.map (div_of_panel ?on_click:f) panel_list

(* ********************************* *)
(* Creating the panel layouts        *)
(* ********************************* *)

(* The "list of panels" div          *)
  
let make_panels_div all_panels_var current_panel_var =
  let$* all_panels = Lwd.get all_panels_var in
  let on_click panel = Some (fun _ev ->
    Lwd.set current_panel_var (Some panel); false) in
  let l = List.map (div_of_panel ~on_click) all_panels in
  div ~a:[a_class (Lwd.pure ["bottom-panel"])] l

(* The "add a panel" form            *)

let make_panel_form panel_form_var =
  let$* class_lwd = Lwd.get panel_form_var in
  div ~a:[a_class (Lwd.pure ["panel-form"; match class_lwd with Some () -> "visible" | None -> "invisible"])] [
      form ~a:[a_action (Lwd.pure "panel"); a_method (Lwd.pure `Post); a_enctype (Lwd.pure "multipart/form-data")] [
          txt (Lwd.pure "Nom : ");
          input ~a:[a_input_type (Lwd.pure `Text); a_name (Lwd.pure "panel_name")] ();
          txt (Lwd.pure "Fichier : ");
          input ~a:[a_input_type (Lwd.pure `File); a_name (Lwd.pure "panel_file")] ();
          txt (Lwd.pure "Soumettre : ");
          input ~a:[a_input_type (Lwd.pure `Submit); a_name (Lwd.pure "new-panel")] ();
        ]
    ]
  
(* The "main panel" div              *)

let make_main_panel_div current_panel_var =
  let$* current_panel_opt:Model.Panel.t option = Lwd.get current_panel_var in
  match current_panel_opt with
    None -> div ~a:[a_class (Lwd.pure ["main-panel"])] []
  | Some current_panel -> 
     div ~a:[a_class (Lwd.pure ["main-panel"])] [img ~src:(Lwd.pure ("img/panel-img/"^current_panel.filename)) ~alt:(Lwd.pure "current panel") ()]

