open Opium
open Tyxml.Html
   
let main_handler _req =
  let myTitle = title (txt ("e-wall")) in
  let div = div ~a:[a_id "app"][] in
  let link = link ~rel:[`Stylesheet]  ~href:"ewall.css" () in
  let myPage =
    html
      (head myTitle [
           script ~a:[a_src "webapp.bc.js"] (txt "");
           link
      ])
      (body [div]) in
  Response.of_html myPage |> Lwt.return 
