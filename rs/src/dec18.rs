use std::fs::File;
use std::io::{self, BufRead};
use std::collections::HashSet;
use std::iter::{Iterator};

type Input<T> = io::BufReader<T>;

pub fn run(file_content: Input<File>) {
    let parsed = parse(file_content);
    println!("Task 1: {}", task1(&parsed));
}

/* Parsing */

fn parse<T: io::Read>(file_content: Input<T>) -> Droplet {
    #[allow(non_snake_case)]
    fn parse_point3D(line: String) -> Point3D {
        let mut digits = line.split(",")
                            .map(|s| s.parse::<Coord>().unwrap());
        Point3D {
            coords: [
                digits.next().unwrap(),
                digits.next().unwrap(),
                digits.next().unwrap()]
        }
    }

    let points = file_content
                    .lines()
                    .map(|r| r.unwrap())
                    .map(parse_point3D)
                    .collect::<HashSet<Point3D>>();

    Droplet { points }
}

/* Point3D */

type Coord = i8; // note: we use unsigned to allow easy arithmetic

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point3D {
    coords: [Coord; 3]
}

impl Point3D {
    fn offset_by(&self, other: &Point3D) -> Point3D {
        let add_dim = |i| {
            self.coords[i] + other.coords[i]
        };

        Point3D {
            coords: [add_dim(0), add_dim(1), add_dim(2)]
        }
    }
}


/* Droplet */

#[derive(Debug)]
struct Droplet {
    points: HashSet<Point3D>
}

impl Droplet {
    fn neighbours(&self, p: Point3D) -> Neighbours {
        Neighbours::new(p)
    }
}

const NUM_NEIGHBOURS: usize = 6;
const NEIGHBOUR_OFFSETS: [Point3D; NUM_NEIGHBOURS] =
    [Point3D { coords: [0, 0, 1] }, Point3D { coords: [0, 0, -1] },
    Point3D { coords: [0, 1, 0] }, Point3D { coords: [0, -1, 0] },
    Point3D { coords: [1, 0, 0] }, Point3D { coords: [-1, 0, 0] }];

struct Neighbours {
    neigh_idx: usize,
    origin:   Point3D,
}

impl Neighbours {
    fn new(origin: Point3D) -> Self {
        Neighbours {
            neigh_idx: 0,
            origin
        }
    }
}

impl Iterator for Neighbours {
    type Item = Point3D;

    fn next(&mut self) -> Option<Self::Item> {
        if self.neigh_idx < NUM_NEIGHBOURS {
            let offset = NEIGHBOUR_OFFSETS[self.neigh_idx];
            let elem = self.origin.offset_by(&offset);
            self.neigh_idx += 1;
            return Some(elem);
        }
        else {
            return None;
        }
    }
}

/* Task 1 */

fn task1(droplet: &Droplet) -> u32 {
    let mut x = 0;
    for p in droplet.points.iter() {
        for q in droplet.neighbours(*p) {
            if !droplet.points.contains(&q) {
                x+=1;
            }
        }
    }
    return x;
}

/* Unit tests */

#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    const EXAMPLE: &str = "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5";

    lazy_static! {
        static ref EXAMPLE_DROPLET: Droplet =
            parse(io::BufReader::new(EXAMPLE.as_bytes()));
    }

    #[test]
    fn validate_example() {
        assert_eq!(task1(&EXAMPLE_DROPLET), 64);
    }

    #[test]
    fn test_neighbours0() {
        let origin = Point3D { coords: [0, 0, 0] };
        let mut neighs = EXAMPLE_DROPLET.neighbours(origin);
        for i in 0..NUM_NEIGHBOURS {
            assert_eq!(neighs.next(), Some(NEIGHBOUR_OFFSETS[i]));
        }
        assert_eq!(neighs.next(), None);
    }

    #[test]
    fn test_neighbours1() {
        let origin = Point3D { coords: [1, 1, 1] };
        let mut neighs = EXAMPLE_DROPLET.neighbours(origin);
        for i in 0..NUM_NEIGHBOURS {
            let mut neighbour = NEIGHBOUR_OFFSETS[i];
            neighbour.coords[0] += 1;
            neighbour.coords[1] += 1;
            neighbour.coords[2] += 1;
            assert_eq!(neighs.next(), Some(neighbour));
        }
        assert_eq!(neighs.next(), None);
    }
}
