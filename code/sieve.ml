let int_sqrt n = n |> float_of_int |> sqrt |> int_of_float

let sieve n =
    let range start stop f =
        List.init (stop - start) (fun x -> f (start + x))
    let replace l pos a = List.mapi (fun i x -> if i = pos then a else x) l in
    let rec sieve_inner j i ns = 
        match j with
        | j when j < n -> sieve_inner (j + i) i (replace ns j Option.none)
        | _ -> ns in
    let rec sieve' ds ns =
        match ds, ns with
        | [], ns -> List.filter_map (fun x -> x) ns
        | i::is, ns -> sieve' is (sieve_inner (2 * i) i ns) in
    sieve' (range 2 (int_sqrt n) (fun x -> x))
           (range 1 n (fun x -> Option.some x))

let _ = sieve 101
    |> List.map string_of_int
    |> String.concat "; "
    |> Format.printf "List of primes below 101: %s\n"
