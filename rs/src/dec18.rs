use std::fs::File;
use std::io::{self, BufRead};
use std::collections::HashSet;

type FileContent = io::BufReader<File>;

pub fn run(file_content: FileContent) {
    println!("{:?}", parse(file_content));
}

type Droplet = HashSet<Point3D>;
type Point3D = (Coord, Coord, Coord);
type Coord   = i8; // note: we use unsigned to allow easy arithmetic

fn parse(file_content: FileContent) -> Droplet {
    #[allow(non_snake_case)]
    fn parse_point3D(line: String) -> Point3D {
        let mut digits = line.split(",")
                            .map(|s| s.parse::<Coord>().unwrap());
        (digits.next().unwrap(),
            digits.next().unwrap(),
            digits.next().unwrap())
    }

    file_content
        .lines()
        .map(|r| r.unwrap())
        .map(parse_point3D)
        .collect::<HashSet<Point3D>>()
}
