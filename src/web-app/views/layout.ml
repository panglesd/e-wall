open Tyxml_lwd.Html
open Lwd.Infix
   
let v = Lwd.var 1


let make () =
  let texte = (Lwd.get v)
               >|= string_of_int
               |> txt in
  let f _ = Lwd.set v (Lwd.peek v + 1) ; false in

  let t = div ~a:[a_class (Lwd.pure ["bli"]); a_onclick (Lwd.pure (Some f))] [ texte ] in t 
