open Js_of_ocaml
open! Tyxml_lwd

let get_element_by_id id =
  Dom_html.window##.document##getElementById (Js.string id)
  |> Js.Opt.to_option
  |> Option.to_result ~none:(Printf.sprintf "No element with id %S." id)

let console_log a = Firebug.console##log a

   
let doc = View.Layout.make ()

let onload _ =
  print_endline "test";
  let main_div = get_element_by_id "app" |> Result.get_ok in
  let (_ : Lwdom.Scheduler.job) = Lwdom.Scheduler.append_to_dom doc main_div in
  Js._false


let _ = print_endline "test2" 

let _ = Dom_html.window##.onload := Dom_html.handler onload
