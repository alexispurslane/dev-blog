type position = { x: float; y: float; z: float }

let main () =
    let entities = Array.init 100000000 (fun i ->
        { x = Random.float 100.0;
          y = Random.float 100.0;
          z = Random.float 100.0 }) in
    let starttime = Unix.gettimeofday () in
    let new_entities = Array.map (fun e -> { e with y = e.y *. 200.0 }) entities in
    let endtime = Unix.gettimeofday () in
    let () = Printf.printf "%f\n%!" (endtime -. starttime) in
    new_entities

let _ = main ()
