use std::fs::File;
use std::io::{self, BufRead};
use std::collections::HashSet;
use std::iter::Iterator;
use std::collections::VecDeque;
use std::hash::Hash;
use std::cmp;
use std::ops;

type Input<T> = io::BufReader<T>;

pub fn run(file_content: Input<File>) {
    let parsed = parse(file_content);

    let res1 = task1(&parsed);
    println!("Task 1: {}", res1);
    assert_eq!(res1, 3650);

    let res2 = task2(&parsed);
    println!("Task 2: {}", res2);
    assert_eq!(res2, 2118);
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
        self.zip_map(other, ops::Add::add)
    }

    fn zip_map<F>(&self, other: &Point3D, f: F) -> Point3D
        where
            F: Fn(Coord, Coord) -> Coord
    {
        let mut newp = *self;
        for i in 0..3 {
            newp.coords[i] = f(self.coords[i], other.coords[i]);
        }
        return newp;
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

    fn is_lava(&self, p: Point3D) -> bool {
        self.points.contains(&p)
    }

    fn is_air(&self, p: Point3D) -> bool {
        !self.is_lava(p)
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

/* BFS */

fn bfs_accum<FA, FN, I, N>(mut accum: FA, neighbours_of: FN, start_node: N) -> ()
    where
        FA: FnMut(&N) -> (),
        FN: Fn(&N) -> I,
        I: Iterator<Item=N>,
        N: Hash + Eq
{
    let mut queue = VecDeque::from([start_node]);
    let mut visited = HashSet::new();

    while let Some(curr_node) = queue.pop_front() {
        if visited.contains(&curr_node) {
            continue;
        }
        for neigh in neighbours_of(&curr_node) {
            queue.push_back(neigh);
        }
        accum(&curr_node);
        visited.insert(curr_node);
    }
}

/* Cube */

/* We reuse Point3D to actually represent the minimum and maximum value
 * of the cube along the different axes. This should represents corners
 * of the cube, but I don't know which ones.
 */
#[derive(Debug)]
struct Cube {
    mins: Point3D,
    maxs: Point3D
}

impl Cube {
    fn enclosing<I: Iterator<Item=Point3D>>(points: &mut I) -> Option<Self> {
        let p1 = points.next()?;

        let mut mins = p1;
        let mut maxs = p1;

        for p in points {
            mins = mins.zip_map(&p, cmp::min);
            maxs = maxs.zip_map(&p, cmp::max);
        }

        let cube = Cube { mins, maxs };
        Some(cube)
    }

    fn any_corner(&self) -> Point3D {
        self.mins
    }

    fn expand(&mut self, amount: Coord) {
        // We decrease each coordinate of min by amount
        // and increase each coordinate of max by amount
        let y = Point3D { coords: [amount; 3] };
        self.mins = self.mins.zip_map(&y, ops::Sub::sub);
        self.maxs = self.maxs.zip_map(&y, ops::Add::add);
    }

    fn contains_point(&self, p: Point3D) -> bool {
         for i in 0..3 {
             let coord_in_cube = (p.coords[i] >= self.mins.coords[i])
                                    && (p.coords[i] <= self.maxs.coords[i]);
             if !coord_in_cube {
                 return false;
             }
         }
         return true;
    }
}

/* Task 2 */
/* We implement task 2 by using the following procedure:
 *   We first compute an enclosing cube around our droplet
 *   We then do BFS within this cube, staying in the "air" part of it
 *   Whenever we touch lava, we increase by one
 *
 * This works because:
 *   BFS will visit every reachable air cube exactly once
 *   For each air cube, if we touch lava on one side, this is a lava face
 *   Because the current air cube will never be reached again, we will count this lava face exactly once
 */

fn task2(droplet: &Droplet) -> u32 {
    let mut cube = Cube::enclosing(&mut droplet.points.iter().map(|p| *p)).unwrap();
    // Add a layer of size 1 such that the corners are necessarily air instead of lava
    cube.expand(1);

    // Run BFS

    let mut surface = 0;
    let increase_surface = |p: &Point3D| {
        // increase the surface by the number of neighbours that are lava
        surface += droplet.neighbours(*p)
                            .filter(|neigh| droplet.is_lava(*neigh))
                            .count() as u32;
    };

    // Return only as neigbours the surrounding air positions
    let bfs_neighbours = |p: &Point3D| {
        droplet.neighbours(*p)
                .filter(|neigh| droplet.is_air(*neigh))
                .filter(|neigh| cube.contains_point(*neigh))
    };

    let start_point = cube.any_corner();

    bfs_accum(increase_surface, bfs_neighbours, start_point);

    return surface;
}

/* Unit tests */

#[cfg(test)]
mod tests {
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
    fn validate_example1() {
        assert_eq!(task1(&EXAMPLE_DROPLET), 64);
    }

    #[test]
    fn validate_example2() {
        assert_eq!(task2(&EXAMPLE_DROPLET), 58);
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
