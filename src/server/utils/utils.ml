open Opium

let () = Stdlib.Random.self_init ()
let get_rand_id () =
  let _ = Stdlib.Random.bits () in
  Uuidm.to_string @@ Uuidm.v4_gen (Stdlib.Random.get_state()) ()
  

let save_file ~prefix ~folder req =
  let open Lwt.Syntax in 
  let files = Hashtbl.create ~random:true 5 in
  let callback ~name:_ ~filename string =
    let filepath = folder^"/"^prefix^(Filename.basename filename) in
    let write file =
      string |> String.length |> Lwt_unix.write_string file string 0 |> Lwt.map ignore
    in
    match Hashtbl.find_opt files filepath with
    | Some file -> write file
    | None ->
      let* file =
        Lwt_unix.openfile filepath Unix.[ O_CREAT; O_TRUNC; O_WRONLY; O_NONBLOCK ] 0o600
      in
      Hashtbl.add files filepath file;
      write file
  in
  let* tab = Request.to_multipart_form_data ~callback req in
  let close filepath file prev =
    let* l = prev in
    let* () = Lwt_unix.close file in
    let new_size = (Unix.stat filepath).st_size - 2 in
    Unix.truncate filepath new_size;
    Lwt.return @@ (Filename.basename filepath) :: l
  in
  let+ filenames = Hashtbl.fold close files (Lwt.return []) in
   (tab, filenames)
