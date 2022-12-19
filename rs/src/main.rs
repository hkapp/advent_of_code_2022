mod dec2;

use std::fs::File;
use std::io;
use std::env;

fn main() {
    let day = env::args().nth(1).expect("No command line arguments passed");
    let filename = gen_filename(&day);
    let file_content = open_file(&filename).unwrap();

    match &day as &str {
        "day2" => dec2::run(file_content),
        _      => panic!("Not implemented yet: {}", day),
    };
}

fn open_file(filename: &str) -> io::Result<io::BufReader<File>> {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file))
}

fn gen_filename(day: &str) -> String {
    format!("../data/{}.data.txt", day)
}
