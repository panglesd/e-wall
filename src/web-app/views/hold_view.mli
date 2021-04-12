open Model
open Tyxml_lwd.Html

val make : Hold.t -> [> Html_types.div ] elt

val hold_in_panel_div : Hold.t Lwd.var -> [> Html_types.div ] Tyxml_lwd.node Lwd_seq.t Lwd.t
