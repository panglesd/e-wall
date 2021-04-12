open Model
open Tyxml_lwd.Html

val div_of_panel : ?on_click:(Panel.t ->
                              (Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t -> bool) option) ->
                   Panel.t -> [> Html_types.div ] elt

val div_list_from_panel_list : ?f:(Panel.t ->
                                 (Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t -> bool) option) ->
                             Panel.t list -> [> Html_types.div ] elt list

val make_panel_list_div : Panel.t list Lwd.var ->
                          Panel.t option Lwd.var -> [> Html_types.div ] Tyxml_lwd.node Lwd_seq.t Lwd.t

(* val loaded : bool Lwd.var *)

val make_main_panel_div : Panel.t option Lwd.var ->
                          Hold.t Lwd.var list Lwd.var ->
                          [> Html_types.div ] Tyxml_lwd.node Lwd_seq.t Lwd.t


val make_panel_form : unit -> [> Html_types.div ] elt
