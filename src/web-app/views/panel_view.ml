open Tyxml_lwd.Html
open Model
open Lwt.Syntax
open Webapp_libs
   
let i = ref 0
let new_panel ():Panel.t = {
    id = string_of_int(!i);
    name = "panel numéro " ^ (string_of_int !i);
    filename = "filename.jpg"
  }
   
let div_from_panel ?f (panel:Panel.t) =
  let panelImg = img
                   ~src:(Lwd.pure @@ "img/panel-img/" ^ (panel.filename))
                   ~alt:(Lwd.pure panel.name)
                   () in
  let panelName = txt (Lwd.pure panel.name) in
  (* match f with
   *   None -> 
   *   div ~a:[a_class (Lwd.pure ["panel"])] [panelImg; panelName]
   * |Some f -> *)
    div ~a:[a_class (Lwd.pure ["panel"]); a_onclick (Lwd.pure f)] [panelImg; panelName]

let div_from_panel_var ?f panel_var =
  Lwd.bind (Lwd.get panel_var) ~f:(div_from_panel ?f)
  
let div_list_from_panel_list ?f panel_list = match f with
    None ->List.map div_from_panel panel_list
   |Some f -> List.map (fun panel -> div_from_panel ~f:(f panel) panel) panel_list

                                        
let panel_var = Lwd.var (new_panel())
let f _ = i := !i+1;
          let _ = let+ new_panel = Request.get_panel 3 in
                  Lwd.set panel_var new_panel in
          false
let panel_div = div_from_panel_var ~f panel_var
