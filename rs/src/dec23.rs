use std::fs::File;
use std::io::{self, BufRead};
use std::collections::{HashMap, HashSet};
use std::slice;
use std::mem;

type Input<T> = io::BufReader<T>;

pub fn run(file_content: Input<File>) {
    let squad = parse(file_content);

    let res1 = task1(squad);
    println!("Task 1: {}", res1);
    //assert_eq!(res1, 88226);

    //let res2 = task2(&map_proj, &moves);
    //println!("Task 2: {}", res2);
    //assert_eq!(res2, 57305);
}

/* Parsing */

fn parse<T: io::Read>(file_content: Input<T>) -> Squad {
    let mut squad = Squad::new();
    let mut north = 0;
    for line in file_content.lines() {
		let mut east = 0;
		for c in line.unwrap().chars() {
			match c {
				'.' => {},
				'#' => {
					let pos = Pos { north, east };
					squad.insert(pos);
				},
				_   => panic!("Unrecognized input character: {:?}", c),
			}
			/* The columns expand eastward */
			east += 1;
		}
		/* The lines expand southward */
		north -= 1;
	}
	return squad;
}

/* Coordinate system */

type Coord = i16;

#[derive(PartialEq, Eq, Debug, Hash, Clone, Copy)]
struct Pos {
	north: Coord,
	east:  Coord,
}

#[derive(Clone, Copy, Debug)]
enum Dir {
	North, South, East, West
}
use Dir::*;

impl Pos {
	fn move_in_dir(&self, dir: Dir) -> Self {
		match dir {
			North => Pos { north: self.north + 1, east: self.east },
			South => Pos { north: self.north - 1, east: self.east },
			East  => Pos { north: self.north, east: self.east + 1 },
			West  => Pos { north: self.north, east: self.east - 1 },
		}
	}

	fn neighbours_in_dir(&self, dir: Dir) -> [Self; 3] {
		let (north_const, east_const) =
			match dir {
				North => (Some(self.north + 1), None),
				South => (Some(self.north - 1), None),
				East  => (None, Some(self.east + 1)),
				West  => (None, Some(self.east - 1)),
			};

		let neigh = |delta| {
			Pos {
				north: north_const.unwrap_or_else(|| self.north + delta),
				east:  east_const.unwrap_or_else(|| self.east + delta),
			}
		};

		[neigh(-1), neigh(0), neigh(1)]
	}
}

/* Circlet */
/* A circlet is a finite ring
 * Its iterator has a beginning and an end
 */

struct Circlet<T> {
	first_idx: usize,
	items:     Box<[T]>
}

impl<T> Circlet<T> {
	fn iter<'a>(&'a self) -> CircletIter<'a, T> {
		CircletIter {
			slice_iter: self.items[self.first_idx..].iter(),
			wrap_iter:  Some(self.items[0..self.first_idx].iter())
						/* Note that this may be empty */
		}
	}

	fn advance(&mut self, steps: usize) {
		self.first_idx = (self.first_idx + steps) % self.items.len();
	}
}

struct CircletIter<'a, T> {
	slice_iter: slice::Iter<'a, T>,
	wrap_iter:  Option<slice::Iter<'a, T>>,
}

impl<'a, T> Iterator for CircletIter<'a, T> {
	type Item = &'a T;

	fn next(&mut self) -> Option<Self::Item> {
		match self.slice_iter.next() {
			Some(x) => Some(x),
			None    => {
				match mem::replace(&mut self.wrap_iter, None) {
					Some(new_iter) => {
						/* Wrap around */
						self.slice_iter = new_iter;
						self.wrap_iter = None;
						self.next()
					}
					None => {
						/* We already wrapped around, the iterator is depleted */
						None
					}
				}
			}
		}
	}
}

impl<T> From<Vec<T>> for Circlet<T> {
	fn from(vec: Vec<T>) -> Self {
		Circlet {
			first_idx: 0,
			items:     vec.into_boxed_slice()
		}
	}
}

/* Phase 1 */

fn has_neighbour_in_dir(elf_pos: Pos, dir: Dir, squad: &Squad) -> bool {
	elf_pos.neighbours_in_dir(dir)
		.iter()
		.any(|neigh| squad.contains(neigh))
}

type RuleBook = Circlet<Dir>;

fn elf_propose(curr_pos: Pos, rule_book: &RuleBook, squad: &Squad) -> Option<Pos> {
	let mut any_neighbours = false;
	let mut next_pos = None;
	for dir in rule_book.iter() {
		if has_neighbour_in_dir(curr_pos, *dir, squad) {
			any_neighbours = true;
		}
		else if next_pos.is_none() {
			next_pos = Some(curr_pos.move_in_dir(*dir));
		}
	}

	if any_neighbours {
		next_pos
	}
	else {
		None
	}
}

type Squad = HashSet<Pos>;

struct Phase1 {
	propositions: Vec<(Pos, Pos)>,
	prop_count:   HashMap<Pos, u16>
}

fn phase1(squad: &Squad, rule_book: &RuleBook) -> Phase1 {
	let mut propositions = Vec::new();
	let mut prop_count = HashMap::new();

	for elf_pos in squad.iter() {
		if let Some(new_pos) = elf_propose(*elf_pos, rule_book, squad) {
			propositions.push((*elf_pos, new_pos));

			// Increase the count for this proposition
			let prev_count = prop_count.remove(&new_pos);
			let new_count = prev_count.unwrap_or(0) + 1;
			prop_count.insert(new_pos, new_count);
		}
	}

	Phase1 {
		propositions,
		prop_count,
	}
}

/* Phase 2 */

fn phase2(squad: &mut Squad, phase1: &Phase1) {
	for prop in &phase1.propositions {
		let (prev_pos, new_pos) = *prop;
		let prop_count = *phase1.prop_count.get(&new_pos).unwrap();
		if prop_count == 1 {
			squad.remove(&prev_pos);
			squad.insert(new_pos);
		}
	}
}

/* Round */

fn elf_round(squad: &mut Squad, rule_book: &mut RuleBook) {
	let p1 = phase1(squad, rule_book);
	phase2(squad, &p1);
	rule_book.advance(1);
}

/* Rect */

struct Rect {
	north_edge: Coord,
	south_edge: Coord,
	east_edge:  Coord,
	west_edge:  Coord
}

macro_rules! rect_edge {
	( $squad:expr, $field:ident, $op:ident ) => {
		$squad.iter()
			.map(|p| p.$field)
			.$op()
			.unwrap()
	}
}

fn enclosing_rect(squad: &Squad) -> Rect {
	Rect {
		north_edge: rect_edge!(squad, north, max),
		south_edge: rect_edge!(squad, north, min),
		east_edge:  rect_edge!(squad, east, max),
		west_edge:  rect_edge!(squad, east, min),
	}
}

impl Rect {
	fn area(&self) -> Coord {
		// Note: if south_edge == north_edge, we still count height=1
		let height = self.north_edge - self.south_edge + 1;
		let width  = self.east_edge - self.west_edge + 1;
		height * width
	}
}

/* Task 1 */

fn init_rule_book() -> RuleBook {
	RuleBook::from(vec![North, South, East, West])
}

fn task1(mut squad: Squad) -> Coord {
	let mut rule_book = init_rule_book();
	for _ in 0..10 {
		elf_round(&mut squad, &mut rule_book);
	}

	let rect = enclosing_rect(&squad);
	rect.area() - squad.len() as Coord
}

/* Unit tests */

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5";

    lazy_static! {
        static ref EXAMPLE_PLANET: MapProjection =
			parse(io::BufReader::new(EXAMPLE.as_bytes())).0;
        static ref EXAMPLE_MOVES: Vec<Move> =
			parse(io::BufReader::new(EXAMPLE.as_bytes())).1;

		static ref EXAMPLE_CUBE: Cube = build_test_cube();
    }
	const A: Pos = Pos { row: 6, column: 12 };
	const B: Pos = Pos { row: 9, column: 15 };
	const C: Pos = Pos { row:12, column: 11 };
	const D: Pos = Pos { row: 8, column:  2 };

	fn build_test_cube() -> Cube {
		/*      +---+
			    | 1 |
		+---+---+---+
		| 2 | 3 | 4 |
		+---+---+---+---+
		        | 5 | 6 |
		        +---+---+
		*/
		let layout =
			[
				vec![None,    None,    Some(1), None],
				vec![Some(2), Some(3), Some(4), None],
				vec![None,    None,    Some(5), Some(6)],
			];

		Cube {
			faces: build_faces_from_layout(4, &layout),
			edges: build_test_edges()
		}
	}

	/*
			 +-------------+
			 :             :
			 : +---------+ :
			 : :         : :
			 : :       +-^-V-+
			 : :   +--->     >-----------+
			 : :   :   |  1  |           :
			 : :   : +-<     <--------+  :
		   +-^-V-+-^-V-+-^-V-+        :  :
	 +----->     >     >     >---+    :  :
	 :     |  2  |  3  |  4  |   :    :  :
	 :  +--<     <     <     <-+ :    :  :
	 :  :  +-^-V-+-^-V-+-^-V-+-^-V-+  :  :
	 :  :    : :   : +->     >     >--+  :
	 :  :    : :   :   |  5  |  6  |     :
	 :  :    : :   +---<     <     <-----+
	 :  :    : :       +-^-V-+-^-V-+
	 :  :    : :         : :   : :
	 :  :    : +---------+ :   : :
	 :  :    :             :   : :
	 :  :    +-------------+   : :
	 :  :                      : :
	 :  +----------------------+ :
	 :                           :
	 +---------------------------+
	*/

	/*
	 When    |      Up        |      Down      |      Left      |      Right     |
	 exiting | Go to | Rotate | Go to | Rotate | Go to | Rotate | Go to | Rotate |
	---------+----------------+----------------+----------------+----------------+
	   1     |   2       x2   |   4       No   |   3       KW   |   6       x2   |
	   2     |   1       x2   |   5       x2   |   6       CW   |   3       No   |
	   3     |   1       CW   |   5       KW   |   2       No   |   4       No   |
	   4     |   1       No   |   5       No   |   3       No   |   6       CW   |
	   5     |   4       No   |   2       x2   |   3       CW   |   6       No   |
	   6     |   4       KW   |   2       KW   |   5       No   |   1       x2   |

	Legend:
	  No: don't rotate
	  CW: clockwise
	  KW: counter-clockwise
	  2x: full turn

	  Note that rotating requires changing the facing direction
	  but also the coordinates when entering the new face.
	*/
	fn build_test_edges() -> CubeEdges {
		use Rotation::*;
		HashMap::from(
			[
				// 1     |   2       x2   |   4       No   |   3       KW   |   6       x2   |
				((1, Up),    Edge { destination: 2, rotation: vec![Clockwise, Clockwise] }),
				((1, Down),  Edge { destination: 4, rotation: vec![] }),
				((1, Left),  Edge { destination: 3, rotation: vec![CounterClockwise] }),
				((1, Right), Edge { destination: 6, rotation: vec![Clockwise, Clockwise] }),
				// 2     |   1       x2   |   5       x2   |   6       CW   |   3       No   |
				((2, Up),    Edge { destination: 1, rotation: vec![Clockwise, Clockwise] }),
				((2, Down),  Edge { destination: 5, rotation: vec![Clockwise, Clockwise] }),
				((2, Left),  Edge { destination: 6, rotation: vec![Clockwise] }),
				((2, Right), Edge { destination: 3, rotation: vec![] }),
				// 3     |   1       CW   |   5       KW   |   2       No   |   4       No   |
				((3, Up),    Edge { destination: 1, rotation: vec![Clockwise] }),
				((3, Down),  Edge { destination: 5, rotation: vec![CounterClockwise] }),
				((3, Left),  Edge { destination: 2, rotation: vec![] }),
				((3, Right), Edge { destination: 4, rotation: vec![] }),
				// 4     |   1       No   |   5       No   |   3       No   |   6       CW   |
				((4, Up),    Edge { destination: 1, rotation: vec![] }),
				((4, Down),  Edge { destination: 5, rotation: vec![] }),
				((4, Left),  Edge { destination: 3, rotation: vec![] }),
				((4, Right), Edge { destination: 6, rotation: vec![Clockwise] }),
				// 5     |   4       No   |   2       x2   |   3       CW   |   6       No   |
				((5, Up),    Edge { destination: 4, rotation: vec![] }),
				((5, Down),  Edge { destination: 2, rotation: vec![Clockwise, Clockwise] }),
				((5, Left),  Edge { destination: 3, rotation: vec![Clockwise] }),
				((5, Right), Edge { destination: 6, rotation: vec![] }),
				// 6     |   4       KW   |   2       KW   |   5       No   |   1       x2   |
				((6, Up),    Edge { destination: 4, rotation: vec![CounterClockwise] }),
				((6, Down),  Edge { destination: 2, rotation: vec![CounterClockwise] }),
				((6, Left),  Edge { destination: 5, rotation: vec![] }),
				((6, Right), Edge { destination: 1, rotation: vec![Clockwise, Clockwise] }),
			]
		)
	}

    #[test]
    fn validate_task1() {
        assert_eq!(task1(&EXAMPLE_PLANET, &EXAMPLE_MOVES), 6032);
    }

    #[test]
    fn validate_task2() {
        assert_eq!(task2_param(&EXAMPLE_PLANET, build_test_cube(), &EXAMPLE_MOVES), 5031);
    }

    #[test]
    fn test_cube_physics() {
		/* Example:
        ...#
        .#..
        #...
        ....
...#.......#
........#..A>-+
..#....#....  :
.D........#.  V
 ^      ...#..B.
 :      .....#..
 :      .#......
 :      ..C...#.
 :        V
 +--------+
        */

        assert_eq!(
			EXAMPLE_CUBE.wrap_around(A, Right),
			(B, Down));
        assert_eq!(
			EXAMPLE_CUBE.wrap_around(C, Down),
			(D, Up));
    }

    #[test]
    fn test_rotate_within_face() {
		/* Clockwise:
           .2.#
           ...3
           1#..
           ..C.

           Counter-clockwise is the opposite
        */
        let p1 = Pos {
			column: C.column - 2,
			row:    C.row - 1,
		};
        let p2 = Pos {
			column: C.column - 1,
			row:    p1.row - 2,
		};
        let p3 = Pos {
			column: C.column + 1,
			row:    p2.row + 1,
		};

		let face = EXAMPLE_CUBE.get_face(identify_face(&EXAMPLE_CUBE, C).unwrap());
		use Rotation::*;

        assert_eq!(
			rotate_within_face(face, C, Clockwise),
			p1);
        assert_eq!(
			rotate_within_face(face, p1, Clockwise),
			p2);
        assert_eq!(
			rotate_within_face(face, p2, Clockwise),
			p3);
        assert_eq!(
			rotate_within_face(face, p3, Clockwise),
			C);

        assert_eq!(
			rotate_within_face(face, C, CounterClockwise),
			p3);
        assert_eq!(
			rotate_within_face(face, p3, CounterClockwise),
			p2);
        assert_eq!(
			rotate_within_face(face, p2, CounterClockwise),
			p1);
        assert_eq!(
			rotate_within_face(face, p1, CounterClockwise),
			C);
    }

    fn test_full_circle(cube: Cube, start_pos: Pos) {
		let mut map_proj = HashMap::new();
		for face in cube.faces.iter() {
			for row in face.top_row..(face.bot_row+1) {
				for column in face.left_col..(face.right_col+1) {
					map_proj.insert(Pos {row, column}, Tile::Open);
				}
			}
		}
		let recurr_time = 50*4;

		let mut astronaut = Astronaut {
			planet: Planet {
				shape_3d:   cube,
				terrain_2d: &map_proj,
			},
			curr_dir: Down,
			curr_pos: start_pos
		};
		astronaut.walk(recurr_time);
		assert_eq!(astronaut.curr_pos, start_pos);

		astronaut.curr_dir = Left;
		astronaut.walk(recurr_time);
		assert_eq!(astronaut.curr_pos, start_pos);

		astronaut.curr_dir = Right;
		astronaut.walk(recurr_time);
		assert_eq!(astronaut.curr_pos, start_pos);

		astronaut.curr_dir = Up;
		astronaut.walk(recurr_time);
		assert_eq!(astronaut.curr_pos, start_pos);
	}

    #[test]
    fn test_real_cube() {
		let cube = Cube::new();
		let start_pos = Pos {
			row:    cube.faces[0].top_row + 1,
			column: cube.faces[0].left_col + 1,
		};
		test_full_circle(cube, start_pos);
    }

}
