open Model
open Tyxml_lwd.Html

let make (hold:Hold.t) = 
  let panel_string = hold.panel.name in 
  div ~a:[a_class (Lwd.pure ["hold-info"])] [
      img ~src:(Lwd.pure "rien") ~alt:(Lwd.pure "alt")();
      div ~a:[a_class (Lwd.pure ["hold-name"])] [txt (Lwd.pure panel_string)];
      div ~a:[a_class (Lwd.pure ["hold-panel-name"])] [txt (Lwd.pure hold.name)];
    ] 
