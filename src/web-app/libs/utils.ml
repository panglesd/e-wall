

let get_img () = match Js_of_ocaml.Dom_html.(getElementById_coerce "main-panel-img" CoerceTo.img) with
    None -> failwith "No main-panel-img"
  | Some elem -> elem

let get_ratio () =
  let img = get_img () in
  let _naturalHeight, naturalWidth = match Js_of_ocaml.Js.Optdef.(
      to_option img##.naturalHeight,
      to_option img##.naturalWidth) with
    | Some n1, Some n2 -> n1, n2
    | _ -> 1,1 in
  Printf.printf "le ratio est : %d / %d\n" naturalWidth img##.width;
  (naturalWidth*100, img##.width*100)
