open Model
open Tyxml_lwd.Html

let make (route:Route.t) = 
  let cotation_string = match route.cotation with
      None -> "Non côtée"
    | Some cot -> Cotation.string_of_cotation cot in
  let feet_string, classList = match route.feet with
      All -> "Tout pied autorisés", "all-feet"
    | Only -> "Seul les prises de main peuvent être utilisées avec les pieds", "restricted-feet" in
  div ~a:[a_class (Lwd.pure ["route-info"; classList])] [
      div ~a:[a_class (Lwd.pure ["route-name"])] [txt (Lwd.pure route.name)];
      div ~a:[a_class (Lwd.pure ["route-cotation"])] [txt (Lwd.pure cotation_string)];
      div ~a:[a_class (Lwd.pure ["route-feet"])] [txt (Lwd.pure feet_string)];
    ] 
