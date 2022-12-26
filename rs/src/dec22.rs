use std::fs::File;
use std::io::{self, BufRead};
use std::collections::HashMap;
use std::str::FromStr;

type Input<T> = io::BufReader<T>;

pub fn run(file_content: Input<File>) {
    let (planet, moves) = parse(file_content);

    let res1 = task1(&planet, &moves);
    println!("Task 1: {}", res1);
    assert_eq!(res1, 88226);

    //let res2 = task2(&parsed);
    //println!("Task 2: {}", res2);
    //assert_eq!(res2, 62744);
}

/* Parsing */

fn parse<T: io::Read>(file_content: Input<T>) -> (Planet, Vec<Move>) {
    let mut line_iter = file_content.lines();

    // Parse the planet
    let mut curr_row = 1;
    let mut planet = Planet::empty();

    while let Some(line_res) = line_iter.next() {
		let line = line_res.unwrap();
		if line.is_empty() {
			// This is the separator
			// What follows is the move sequence
			break;
		}

		parse_planet_row(&mut planet, line, curr_row);
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

	return (planet, moves);
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

fn parse_planet_row(planet: &mut Planet, row_str: String, row_idx: Coord) {
	let mut curr_col = 1;
	for c in row_str.chars() {
		let curr_pos = Pos::from_row_col(row_idx, curr_col);
		match c {
			' ' => {},
			'.' => { planet.terrain.insert(curr_pos, Tile::Open); },
			'#' => { planet.terrain.insert(curr_pos, Tile::Blocked); },
			_   => { panic!("Unknown tile: {:?}", c); },
		}
		curr_col += 1;
	}
}

/* Coordinate system */

/*
  1--x-->
  |
  y
  |
  V
*/

/* Note: both coordinates start at 1
 * So doing Coord - 1 is always ok, as long as we check for wrap around immediately
 */
type Coord = u16;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Pos {
	x: Coord,
	y: Coord
}

#[derive(Debug, Clone, Copy)]
enum Dir {
	Left, Right, Up, Down
}
use Dir::*;

impl Pos {
	fn from_row_col(row: Coord, col: Coord) -> Pos {
		Pos {
			x: col,
			y: row
		}
	}

	fn move_dir(&self, dir: Dir) -> Pos {
		let x = self.x;
		let y = self.y;

		match dir {
			Left  => Pos { x: x-1, y },
			Right => Pos { x: x+1, y },
			Up    => Pos { x, y: y-1 },
			Down  => Pos { x, y: y+1 },
		}
	}

	fn column(&self) -> Coord {
		self.x
	}

	fn row(&self) -> Coord {
		self.y
	}

	fn with_column(&self, new_col: Coord) -> Pos {
		Pos {
			x: new_col,
			y: self.y
		}
	}

	fn with_row(&self, new_row: Coord) -> Pos {
		Pos {
			x: self.x,
			y: new_row
		}
	}
}

/* Planet */
/* A planet is an object we can walk on
 * Going in the same direction eventually comes back around
 */
// Note: we could turn the HashMap into a bitset
struct Planet {
	terrain: HashMap<Pos, Tile>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tile {
	Blocked,
	Open
}

impl Planet {
	fn empty() -> Self {
		Planet {
			terrain: HashMap::new()
		}
	}

	fn next_pos_in_dir(&self, curr_pos: Pos, dir: Dir) -> Pos {
		let unchecked = curr_pos.move_dir(dir);
		if !self.terrain.contains_key(&unchecked) {
			// Need to wrap around
			assert!(self.terrain.contains_key(&curr_pos));
			self.wrap_around(unchecked, dir)
		}
		else {
			// No need to wrap around
			unchecked
		}
	}

	fn wrap_around(&self, pos: Pos, dir: Dir) -> Pos {
		use Dir::*;
		match dir {
			Left  => {
				/* New column is the maximum column for this row */
				let new_col = self.max_col_on_row(pos.row());
				pos.with_column(new_col)
			}
			Right => {
				let new_col = self.min_col_on_row(pos.row());
				pos.with_column(new_col)
			}
			Up => {
				let new_row = self.max_row_on_col(pos.column());
				pos.with_row(new_row)
			}
			Down => {
				let new_row = self.min_row_on_col(pos.column());
				pos.with_row(new_row)
			}
		}
	}

	// TODO cache all of these
	fn max_col_on_row(&self, row_idx: Coord) -> Coord {
		self.compute_wraparound(row_idx, true, true)
	}

	fn min_col_on_row(&self, row_idx: Coord) -> Coord {
		self.compute_wraparound(row_idx, true, false)
	}

	fn max_row_on_col(&self, col_idx: Coord) -> Coord {
		self.compute_wraparound(col_idx, false, true)
	}

	fn min_row_on_col(&self, col_idx: Coord) -> Coord {
		self.compute_wraparound(col_idx, false, false)
	}

	fn compute_wraparound(&self, idx: Coord, on_rows: bool, do_max: bool) -> Coord {
		let fixed_part  = |p: &Pos| if on_rows { p.row() } else { p.column() };
		let moving_part = |p: &Pos| if on_rows { p.column() } else { p.row() };

		let mult = if do_max { 1 } else { -1 };

		self.terrain
			.keys()
			.filter(|p| fixed_part(p) == idx)
			.map(|p| moving_part(p))
			.max_by_key(|z| mult * (*z as i16))
			.unwrap()
	}

	fn is_free_at(&self, pos: Pos) -> bool {
		self.terrain.get(&pos) == Some(&Tile::Open)
	}
}

/* Astronaut */
/* Someone who walks on planets */

struct Astronaut<'a> {
	planet:   &'a Planet,
	curr_pos: Pos,
	curr_dir: Dir
}

impl<'a> Astronaut<'a> {
	fn walk(&mut self, steps: Coord) {
		for _ in 0..steps {
			let moved = self.walk_one_step();
			if !moved {
				break;
			}
		}
	}

	fn walk_one_step(&mut self) -> bool {
		let next_pos = self.planet.next_pos_in_dir(self.curr_pos, self.curr_dir);
		if self.planet.is_free_at(next_pos) {
			self.curr_pos = next_pos;
			return true;
		}
		else {
			return false;
		}
	}

	fn turn(&mut self, rotation: Rotation) {
		self.curr_dir = rotation.apply_to(self.curr_dir);
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

fn init_pos(planet: &Planet) -> Pos {
	let row = 1;
	let col = planet.min_col_on_row(row);
	Pos::from_row_col(row, col)
}

const INIT_DIR: Dir = Dir::Right;

fn task1(planet: &Planet, moves: &[Move]) -> i32 {
	let mut astronaut = Astronaut {
		planet,
		curr_pos: init_pos(planet),
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

	1000 * (astronaut.curr_pos.row() as i32)
		+ 4 * (astronaut.curr_pos.column() as i32)
		+ pwd_value(astronaut.curr_dir)
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
        static ref EXAMPLE_PLANET: Planet =
			parse(io::BufReader::new(EXAMPLE.as_bytes())).0;
        static ref EXAMPLE_MOVES: Vec<Move> =
			parse(io::BufReader::new(EXAMPLE.as_bytes())).1;
    }

    #[test]
    fn validate_task1() {
        assert_eq!(task1(&EXAMPLE_PLANET, &EXAMPLE_MOVES), 6032);
    }

}
