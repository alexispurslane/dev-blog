extern crate rayon;
use rayon::prelude::*;

struct Position {
    x: f64,
    y: f64,
    z: f64
}

fn rand_range(top: f64) -> f64 {
    let nanos = std::time::UNIX_EPOCH
        .elapsed()
        .unwrap()
        .subsec_nanos() as f64;
    nanos % top
}

fn main() {
    let mut entities = (0..100_000_000).map(|_| Position { 
        x: rand_range(100.0),
        y: rand_range(100.0),
        z: rand_range(100.0),
    }).collect::<Vec<_>>();

    let start_time = std::time::UNIX_EPOCH.elapsed().unwrap().as_millis();
    entities.par_iter_mut().for_each(|e| {
        e.y *= 200.0;
    });
    let end_time = std::time::UNIX_EPOCH.elapsed().unwrap().as_millis();
    println!("{}", (end_time - start_time) as f64 / 1000.0);
}

