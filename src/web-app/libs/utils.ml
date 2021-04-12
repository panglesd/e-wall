

let get_img () = match Js_of_ocaml.Dom_html.(getElementById_coerce "main-panel-img" CoerceTo.img) with
    None -> failwith "No main-panel-img"
  | Some elem -> elem
