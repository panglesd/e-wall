open Opium
open Tyxml.Html

let myTitle = title (txt ("e-wall"))
let div = div ~a:[a_id "app"][txt "hello, my worlds!"]
let myPage =
  html
    (head myTitle [
         script ~a:[a_src "webapp.bc.js"] (txt "")
    ])
    (body [div])
   
let hello _req = Response.of_plain_text "Hello World" |> Lwt.return

let hello2 _req = Response.of_html myPage |> Lwt.return

let greet req =
  let name = Router.param req "name" in
  Printf.sprintf "Hello, %s" name |> Response.of_plain_text |> Lwt.return







  
let start () =
  App.empty
  |> App.get "/" hello
  |> App.get "/h" hello2
  |> App.get "/greet/:name" greet
  |> App.middleware @@ Middleware.static_unix ~local_path:"_build/default/src/web-app/" ~uri_prefix:"/" ()
  |> App.run_command
  |> ignore
let myRes = Response.of_html ~indent:true
