
type position = { mutable x: float; mutable y: float; mutable z: float }

let main () =
    let entities = Array.init 100000000 (fun i ->
        { x = Random.float 100.0;
          y = Random.float 100.0;
          z = Random.float 100.0 }) in
    let starttime = Unix.gettimeofday () in
    let () = for i = 0 to Array.length entities - 1 do
        (Array.get entities i).y <- (Array.get entities i).y *. 200.0
    done in
    let endtime = Unix.gettimeofday () in
    Printf.printf "%f\n%!" (endtime -. starttime)

let _ = main ()
