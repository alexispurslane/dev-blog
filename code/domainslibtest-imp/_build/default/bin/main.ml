type position = { mutable x: float; mutable y: float; mutable z: float }

let main () =
    let pool = Domainslib.Task.setup_pool ~num_domains:20 () in
    let entities = Array.init 100000000 (fun _ ->
        { x = Random.float 100.0;
          y = Random.float 100.0;
          z = Random.float 100.0 }) in
    let dotask = fun _ ->
        Domainslib.Task.parallel_for
                ~start:0
                ~finish:(Array.length entities - 1) 
                ~body:(fun i -> entities.(i).y <- entities.(i).y *. 200.0) pool
    in
    let starttime = Unix.gettimeofday () in
    let _ = Domainslib.Task.run pool dotask in
    let endtime = Unix.gettimeofday () in
    Domainslib.Task.teardown_pool pool;
    Printf.printf "%f\n%!" (endtime -. starttime)

let _ = main ()
