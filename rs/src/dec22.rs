use std::fs::File;
use std::io::{self, BufRead};
use std::collections::HashMap;
use std::str::FromStr;

type Input<T> = io::BufReader<T>;

pub fn run(file_content: Input<File>) {
    let (map_proj, moves) = parse(file_content);

    let res1 = task1(&map_proj, &moves);
    println!("Task 1: {}", res1);
    assert_eq!(res1, 88226);

    let res2 = task2(&map_proj, &moves);
    println!("Task 2: {}", res2);
    //assert_eq!(res2, 62744);
}

/* Parsing */

fn parse<T: io::Read>(file_content: Input<T>) -> (MapProjection, Vec<Move>) {
    let mut line_iter = file_content.lines();

    // Parse the planet
    let mut curr_row = 1;
    let mut map_proj = MapProjection::new();

    while let Some(line_res) = line_iter.next() {
		let line = line_res.unwrap();
		if line.is_empty() {
			// This is the separator
			// What follows is the move sequence
			break;
		}

		parse_map_proj_row(&mut map_proj, line, curr_row);
		curr_row += 1;
	}

	// Parse the move sequence
	let move_line = line_iter.next().unwrap().unwrap();
	assert!(line_iter.next().is_none());
	let mut moves = Vec::new();
	for segment in segregate_str(&move_line, char::is_ascii_digit) {
		let m =
			match segment {
				Ok(digits) => Move::Walk(digits.parse().unwrap()),
				Err(chars) => Move::Turn(chars.parse().unwrap()),
			};
		moves.push(m);
	}

	return (map_proj, moves);
}

impl FromStr for Rotation {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
		match s {
			"R" => Ok(Rotation::Clockwise),
			"L" => Ok(Rotation::CounterClockwise),
			_   => Err(format!("Invalid rotation: {:?}", s)),
		}
	}
}

fn segregate_str<P>(s: &str, pred: P) -> SegregateStr<P>
	where
		P: Fn(&char) -> bool
{
	SegregateStr {
		remaining: s,
		pred
	}
}

struct SegregateStr<'a, P> {
	remaining: &'a str,
	pred:      P
}

impl<'a, P> Iterator for SegregateStr<'a, P>
	where P: Fn(&char) -> bool
{
	type Item = Result<&'a str, &'a str>;

	fn next(&mut self) -> Option<Self::Item> {
		if self.remaining.is_empty() {
			return None;
		}

		let first_char = self.remaining.chars().next().unwrap();
		let const_res = (self.pred)(&first_char);
		let split_idx = self.remaining
							.chars()
							.take_while(|c| (self.pred)(c) == const_res)
							.count();

		let (returned, new_remaining) = self.remaining.split_at(split_idx);
		self.remaining = new_remaining;

		match const_res {
			true  => Some(Ok(returned)),
			false => Some(Err(returned)),
		}
	}
}

#[allow(dead_code)]
fn segregate<T, P>(elems: &[T], pred: P) -> Segregate<T, P>
	where
		P: Fn(&T) -> bool
{
	Segregate {
		remaining: elems,
		pred
	}
}

struct Segregate<'a, T, P> {
	remaining: &'a [T],
	pred:      P
}

impl<'a, T, P> Iterator for Segregate<'a, T, P>
	where P: Fn(&T) -> bool
{
	type Item = Result<&'a [T], &'a [T]>;

	fn next(&mut self) -> Option<Self::Item> {
		if self.remaining.is_empty() {
			return None;
		}

		let const_res = (self.pred)(&self.remaining[0]);
		let mut curr_idx = 1;
		while curr_idx < self.remaining.len() {
			if (self.pred)(&self.remaining[curr_idx]) == const_res {
				// Same result: expand and continue
				curr_idx += 1;
				continue;
			}
			else {
				// Different result: build and return
				// We want to include up until the previous element only
				curr_idx -= 1;
				break;
			}
		}

		let (returned, new_remaining) = self.remaining.split_at(curr_idx+1);
		self.remaining = new_remaining;

		match const_res {
			true  => Some(Ok(returned)),
			false => Some(Err(returned)),
		}
	}
}

fn parse_map_proj_row(map_proj: &mut MapProjection, row_str: String, row_idx: Coord) {
	let mut curr_col = 1;
	for c in row_str.chars() {
		let curr_pos = Pos { row: row_idx, column: curr_col };
		match c {
			' ' => {},
			'.' => { map_proj.insert(curr_pos, Tile::Open); },
			'#' => { map_proj.insert(curr_pos, Tile::Blocked); },
			_   => { panic!("Unknown tile: {:?}", c); },
		}
		curr_col += 1;
	}
}

/* Map projection: 2D coordinate system */

/*
  1--c-->
  |
  r
  |
  V
*/

// Note: we use unsigned for easy arithmetics
type Coord = i16;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Pos {
	row:    Coord,
	column: Coord
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Dir {
	Left, Right, Up, Down
}
use Dir::*;

impl Pos {
	fn move_dir(&self, dir: Dir) -> Pos {
		let row    = self.row;
		let column = self.column;

		match dir {
			Left  => Pos { column: column-1, row },
			Right => Pos { column: column+1, row },
			Up    => Pos { column,    row: row-1 },
			Down  => Pos { column,    row: row+1 },
		}
	}

	fn with_column(&self, new_col: Coord) -> Pos {
		Pos {
			column: new_col,
			row:    self.row
		}
	}

	fn with_row(&self, new_row: Coord) -> Pos {
		Pos {
			row:    new_row,
			column: self.column
		}
	}
}

type MapProjection = HashMap<Pos, Tile>;

/* Planet */
/* A planet is an object we can walk on
 * Going in the same direction eventually comes back around
 */
struct Planet<'a, S> {
	terrain_2d: &'a MapProjection,
	shape_3d:   S
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tile {
	Blocked,
	Open
}

impl<'a, S> Planet<'a, S> {
	fn is_free_at(&self, pos: Pos) -> bool {
		self.terrain_2d.get(&pos) == Some(&Tile::Open)
	}
}

impl <'a, S: Shape> Planet<'a, S> {
	fn next_pos_in_dir(&self, curr_pos: Pos, dir: Dir) -> (Pos, Dir) {
		let unchecked_new_pos = curr_pos.move_dir(dir);
		if !self.terrain_2d.contains_key(&unchecked_new_pos) {
			// Need to wrap around
			assert!(self.terrain_2d.contains_key(&curr_pos));
			self.shape_3d.wrap_around(curr_pos, dir)
		}
		else {
			// No need to wrap around
			(unchecked_new_pos, dir)
		}
	}
}

/* Astronaut */
/* Someone who walks on planets */

struct Astronaut<'a, S> {
	planet:   Planet<'a, S>,
	curr_pos: Pos,
	curr_dir: Dir
}

impl<'a, S> Astronaut<'a, S> {
	fn turn(&mut self, rotation: Rotation) {
		self.curr_dir = rotation.apply_to(self.curr_dir);
	}
}

impl<'a, S: Shape> Astronaut<'a, S> {
	fn walk(&mut self, steps: Coord) {
		for _ in 0..steps {
			let moved = self.walk_one_step();
			if !moved {
				break;
			}
		}
	}

	fn walk_one_step(&mut self) -> bool {
		let (next_pos, next_dir) =
			self.planet.next_pos_in_dir(self.curr_pos, self.curr_dir);

		if self.planet.is_free_at(next_pos) {
			self.curr_pos = next_pos;
			self.curr_dir = next_dir;
			return true;
		}
		else {
			return false;
		}
	}
}

/* 3D Shape */

trait Shape {
	fn wrap_around(&self, pos: Pos, dir: Dir) -> (Pos, Dir);
}

struct Torus<'a> {
	map_projection: &'a MapProjection
}

impl<'a> Shape for Torus<'a> {

	fn wrap_around(&self, pos: Pos, dir: Dir) -> (Pos, Dir) {
		/* To wrap around, one coordinate remains fixed and one will change. */
		use Dir::*;

		/* The coordinate that remains fixed depends on the direction */
		let fixed_row = (dir == Left) || (dir == Right);
		let fixed_part  = |p: &Pos| if fixed_row { p.row } else { p.column };
		let moving_part = |p: &Pos| if fixed_row { p.column } else { p.row };
		let const_coord = fixed_part(&pos);

		/* The moving coordinate becomes either min or max, depending on the dir */
		let do_max = (dir == Left) || (dir == Up);
		let mult = if do_max { 1 } else { -1 };

		let new_moving =
			self.map_projection
				.keys()
				.filter(|p| fixed_part(p) == const_coord)
				.map(|p| moving_part(p))
				.max_by_key(|z| mult * (*z as i16))
				.unwrap();

		let new_pos = if fixed_row { pos.with_column(new_moving) }
					  else { pos.with_row(new_moving) };

		(new_pos, dir)
	}
}

/* Task 1 */

enum Move {
	Walk(Coord),
	Turn(Rotation)
}

#[derive(Clone, Copy)]
enum Rotation {
	Clockwise,
	CounterClockwise
}

impl Rotation {
	fn apply_to(&self, dir: Dir) -> Dir {
		use Rotation::*;
		use Dir::*;
		match self {
			Clockwise =>
				match dir {
					Right => Down,
					Down  => Left,
					Left  => Up,
					Up    => Right,
				}
			CounterClockwise =>
				match dir {
					Right => Up,
					Up    => Left,
					Left  => Down,
					Down  => Right,
				}
		}
	}
}

fn init_pos(map_proj: &MapProjection) -> Pos {
	let row = 1;
	let col = map_proj.keys()
				.filter(|p| p.row == row)
				.map(|p| p.column)
				.min()
				.unwrap();
	Pos { row: row, column: col }
}

const INIT_DIR: Dir = Dir::Right;

fn find_password<S: Shape>(map_proj: &MapProjection, shape: S, moves: &[Move]) -> i32 {
	let planet = Planet {
		terrain_2d: map_proj,
		shape_3d:   shape,
	};

	let mut astronaut = Astronaut {
		planet,
		curr_pos: init_pos(map_proj),
		curr_dir: INIT_DIR,
	};

	for m in moves {
		use Move::*;
		match m {
			Walk(steps) => astronaut.walk(*steps),
			Turn(dir)   => astronaut.turn(*dir),
		}
	}

	// Compute final result
	fn pwd_value(dir: Dir) -> i32 {
		use Dir::*;
		match dir {
			Right => 0,
			Down  => 1,
			Left  => 2,
			Up    => 3,
		}
	}

	1000 * (astronaut.curr_pos.row as i32)
		+ 4 * (astronaut.curr_pos.column as i32)
		+ pwd_value(astronaut.curr_dir)
}

fn task1(map_proj: &MapProjection, moves: &[Move]) -> i32 {
	find_password(map_proj,
		Torus { map_projection: map_proj },
		moves)
}

/* Cube */

struct Cube {
	faces: CubeFaces,
	edges: HashMap<(FaceId, Dir), Edge>
}

impl Cube {
	fn new(side_len: Coord) -> Self {
		Cube {
			faces: build_face_corners(side_len),
			edges: build_edges()
		}
	}

	fn get_face(&self, face_id: FaceId) -> &Face {
		&self.faces[face_id as usize - 1]
	}
}

/*
                 +-----+<---1
                 |     |
                 |  1  |
                 |     |
   +-----++-----++-----+<---l
   +-----++-----++-----+<---l+1
   |     ||     ||     |
   |  2  ||  3  ||  4  |
   |     ||     ||     |
   +-----++-----++-----++-----+<---2l
   +-----++-----++-----++-----+<---2l+1
   ^     ^^     ^|     ||     |
   |     ||     ||  5  ||  6  |
   1     l|     ||     ||     |
          |     |+-----++-----+<---3l
         l+1    |^     ^^     ^
               2l|     ||     |
                 |     ||     4l
              2l+1    3l|
                        3l+1
*/

const NUM_FACES: usize = 6;
type CubeFaces = [Face; NUM_FACES];

fn build_face_corners(side_len: Coord) -> CubeFaces {
	let corners_row_col = |proj_row: Coord, proj_col: Coord| {
		let top_row = (proj_row - 1) * side_len + 1;
		let bot_row = proj_row * side_len;

		let left_col = (proj_col - 1) * side_len + 1;
		let right_col = proj_col * side_len;

		Face { left_col, right_col, top_row, bot_row }
	};

	[
		/*1*/ corners_row_col(1, 3),
		/*2*/ corners_row_col(2, 1),
		/*3*/ corners_row_col(2, 2),
		/*4*/ corners_row_col(2, 3),
		/*5*/ corners_row_col(3, 3),
		/*6*/ corners_row_col(3, 4),
	]
}

type FaceId = u8;
struct Face {
	left_col:  Coord,
	right_col: Coord,
	top_row:   Coord,
	bot_row:   Coord,
}

fn belongs_to_face(face: &Face, pos: Pos) -> bool {
	pos.row >= face.top_row
	&& pos.row <= face.bot_row
	&& pos.column >= face.left_col
	&& pos.column <= face.right_col
}

fn identify_face(cube: &Cube, pos: Pos) -> Option<FaceId> {
	cube.faces
		.iter()
		.enumerate()
		.find(|(_idx, c)| belongs_to_face(c, pos))
		.map(|(idx, _c)| idx as FaceId + 1)
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

struct Edge {
	destination: FaceId,
	rotation:    Vec<Rotation>
}

type CubeEdges = HashMap<(FaceId, Dir), Edge>;

// TODO we could turn this into a lazy const
fn build_edges() -> CubeEdges {
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

fn rotate_within_face(curr_face: &Face, pos: Pos, rotation: Rotation) -> Pos {
	// Note: at most two can be true at a time (if pos is one a corner)
	let on_top_row = pos.row == curr_face.top_row;
	let on_bot_row = pos.row == curr_face.bot_row;
	let on_left_col  = pos.column == curr_face.left_col;
	let on_right_col = pos.column == curr_face.right_col;

	use Rotation::*;
	match rotation {
		Clockwise => {
			/* +--1+
			 * 4   |
			 * |   2
			 * +3--+
			 */
			if on_top_row {
				/* 1 -> 2 */
				Pos {
					row:    (pos.column - curr_face.left_col) + curr_face.top_row,
					column: curr_face.right_col
				}
			}
			else if on_right_col {
				/* 2 -> 3 */
				Pos {
					row:    curr_face.bot_row,
					column: curr_face.left_col + (curr_face.bot_row - pos.row)
				}
			}
			else if on_bot_row {
				/* 3 -> 4 */
				Pos {
					row:    curr_face.top_row + (pos.column - curr_face.left_col),
					column: curr_face.left_col
				}
			}
			else if on_left_col {
				/* 4 -> 1 */
				Pos {
					row:    curr_face.top_row,
					column: curr_face.left_col + (curr_face.bot_row - pos.row)
				}
			}
			else {
				unreachable!()
			}
		}
		CounterClockwise => {
			/* +--1+
			 * 2   |
			 * |   4
			 * +3--+
			 */
			if on_top_row {
				/* 1 -> 2 */
				Pos {
					row:    (curr_face.right_col - pos.column) + curr_face.top_row,
					column: curr_face.left_col
				}
			}
			else if on_left_col {
				/* 2 -> 3 */
				Pos {
					row:    curr_face.bot_row,
					column: curr_face.left_col + (pos.row - curr_face.top_row)
				}
			}
			else if on_bot_row {
				/* 3 -> 4 */
				Pos {
					row:    curr_face.top_row + (curr_face.right_col - pos.column),
					column: curr_face.right_col
				}
			}
			else if on_right_col {
				/* 4 -> 1 */
				Pos {
					row:    curr_face.top_row,
					column: curr_face.left_col + (pos.row - curr_face.top_row)
				}
			}
			else {
				unreachable!()
			}
		}
	}
}

fn cross_edge(
	cube:      &Cube,
	edge:      &Edge,
	prev_face: &Face,
	prev_pos:   Pos,
	prev_dir:   Dir)
	-> (Pos, Dir)
{
	/* What do we need to do?
	 * Rotate the direction
	 * Compute the new position
	 * 	Compute the position in the previous face
	 *  Rotate that position within the face
	 *  Move the new point to the new face
	 */
	let mut new_pos = prev_pos;
	let mut new_dir = prev_dir;
	for rotation in edge.rotation.iter() {
		new_dir = rotation.apply_to(new_dir);
		new_pos = rotate_within_face(prev_face, new_pos, *rotation);
	}
	// Translate the point to the new face
	// Take into account the step we're making
	let new_face = cube.get_face(edge.destination);
	new_pos = enter_face(new_pos, new_dir, prev_face, new_face);
	(new_pos, new_dir)
}

fn enter_face(pos: Pos, dir: Dir, prev_face: &Face, new_face: &Face) -> Pos {
	/*     +-3---+
	       |     4
	       2     |
	 +----1+----1+
	 4<   ^|
	 | V  >2
	 +-3---+
	 */
	match dir {
		Up => Pos {
			row:    new_face.bot_row,
			column: pos.column - prev_face.left_col + new_face.left_col
		},
		Right => Pos {
			column: new_face.left_col,
			row:    pos.row - prev_face.top_row + new_face.top_row
		},
		Down => Pos {
			row:    new_face.top_row,
			column: pos.column - prev_face.left_col + new_face.left_col
		},
		Left => Pos {
			column: new_face.right_col,
			row:    pos.row - prev_face.top_row + new_face.top_row
		}
	}
}

impl Shape for Cube {
	fn wrap_around(&self, pos: Pos, dir: Dir) -> (Pos, Dir) {
		let curr_face = identify_face(&self, pos)
							.expect(&format!("Can't find the face for {:?}", pos));
		let edge = self.edges.get(&(curr_face, dir)).unwrap();
		// Trivial edges should never reach here
		assert!(!edge.rotation.is_empty());
		cross_edge(&self, edge, self.get_face(curr_face), pos, dir)
	}
}

/* Task 2 */

fn task2_param(map_proj: &MapProjection, cube_side_len: Coord, moves: &[Move]) -> i32 {
	find_password(map_proj, Cube::new(cube_side_len), moves)
}

fn task2(map_proj: &MapProjection, moves: &[Move]) -> i32 {
	task2_param(map_proj, 50, moves)
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

		static ref EXAMPLE_CUBE: Cube = Cube::new(4);
    }
	const A: Pos = Pos { row: 6, column: 12 };
	const B: Pos = Pos { row: 9, column: 15 };
	const C: Pos = Pos { row:12, column: 11 };
	const D: Pos = Pos { row: 8, column:  2 };

    #[test]
    fn validate_task1() {
        assert_eq!(task1(&EXAMPLE_PLANET, &EXAMPLE_MOVES), 6032);
    }

    #[test]
    fn validate_task2() {
        assert_eq!(task2_param(&EXAMPLE_PLANET, 4, &EXAMPLE_MOVES), 5031);
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
		/* Clokwise:
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

}
