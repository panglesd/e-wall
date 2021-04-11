


let () = Stdlib.Random.self_init ()

let get_rand_id () =
    let _ = Stdlib.Random.bits () in
    Uuidm.to_string @@ Uuidm.v4_gen (Stdlib.Random.get_state()) ()
    ;
