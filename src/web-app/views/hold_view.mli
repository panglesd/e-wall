open Model
open Tyxml_lwd.Html

val make : Hold.t Lwd.var -> [> Html_types.div ] elt

val hold_in_panel_div : Hold.t Lwd.var -> [> Html_types.div ] Tyxml_lwd.node Lwd_seq.t Lwd.t

val make_hold_list_div : [> Html_types.div ] Tyxml_lwd.node Lwd_seq.t Lwd.t
