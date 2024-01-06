type position = { x: float; y: float; z: float }

let parallel_map f pool arr =
  let res = Array.make (Array.length arr) (f arr.(0)) in
  Domainslib.Task.parallel_for ~start:0 ~finish:(Array.length arr - 1) 
    ~body:(fun i -> res.(i) <- f arr.(i)) pool;
  Array.to_list res

let main () =
    let pool = Domainslib.Task.setup_pool ~num_domains:20 () in
    let entities = Array.init 100000000 (fun _ ->
        { x = Random.float 100.0;
          y = Random.float 100.0;
          z = Random.float 100.0 }) in
    let create_new_entities = parallel_map (fun e -> { e with y = e.y *. 200.0
        }) pool in
    let starttime = Unix.gettimeofday () in
    let _ = Domainslib.Task.run pool (fun _ -> create_new_entities entities) in
    let endtime = Unix.gettimeofday () in
    Domainslib.Task.teardown_pool pool;
    Printf.printf "%f\n%!" (endtime -. starttime)

let _ = main ()
