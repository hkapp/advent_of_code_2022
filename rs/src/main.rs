mod dec2;
mod dec18;
mod dec19;
mod dec20;
mod dec21;
mod dec22;
mod dec23;
mod dec24;
mod dec25;

mod astar;

#[cfg(test)]
#[macro_use]
extern crate lazy_static;

use std::fs::File;
use std::io;
use std::env;

fn main() {
    let day = env::args().nth(1).expect("No command line arguments passed");
    let filename = gen_filename(&day);
    let file_content = open_file(&filename).unwrap();

    match &day as &str {
        "day2"  => dec2::run(file_content),
        "day18" => dec18::run(file_content),
        "day19" => dec19::run(file_content),
        "day20" => dec20::run(file_content),
        "day21" => dec21::run(file_content),
        "day22" => dec22::run(file_content),
        "day23" => dec23::run(file_content),
        "day24" => dec24::run(file_content),
        "day25" => dec25::run(file_content),
        _       => panic!("Not implemented yet: {}", day),
    };
}

fn open_file(filename: &str) -> io::Result<io::BufReader<File>> {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file))
}

fn gen_filename(day: &str) -> String {
    format!("../data/{}.data.txt", day)
}
