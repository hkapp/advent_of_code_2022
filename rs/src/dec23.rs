use std::fs::File;
use std::io::{self, BufRead};
use std::collections::{HashMap, HashSet};
use std::slice;
use std::mem;

type Input<T> = io::BufReader<T>;

pub fn run(file_content: Input<File>) {
    let squad = parse(file_content);

    let res1 = task1(squad.clone());
    println!("Task 1: {}", res1);
    assert_eq!(res1, 4056);

    let res2 = task2(squad);
    println!("Task 2: {}", res2);
    assert_eq!(res2, 999);
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

type Count = u16;

fn phase2(squad: &mut Squad, phase1: &Phase1) -> Count {
	let mut move_count = 0;
	for prop in &phase1.propositions {
		let (prev_pos, new_pos) = *prop;
		let prop_count = *phase1.prop_count.get(&new_pos).unwrap();
		if prop_count == 1 {
			squad.remove(&prev_pos);
			squad.insert(new_pos);
			move_count += 1;
		}
	}
	return move_count;
}

/* Round */

fn elf_round(squad: &mut Squad, rule_book: &mut RuleBook) -> Count {
	let p1 = phase1(squad, rule_book);
	let move_count = phase2(squad, &p1);
	rule_book.advance(1);
	return move_count;
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
		println!("height: {}, width: {}", height, width);
		height * width
	}
}

/* Task 1 */

fn init_rule_book() -> RuleBook {
	RuleBook::from(vec![North, South, West, East])
}

fn task1(mut squad: Squad) -> Coord {
	let mut rule_book = init_rule_book();
	for _ in 0..10 {
		elf_round(&mut squad, &mut rule_book);
	}

	let rect = enclosing_rect(&squad);
	show_squad_within(&squad, &rect);
	rect.area() - squad.len() as Coord
}

fn show_squad_within(squad: &Squad, rect: &Rect) {
	for north in (rect.south_edge..rect.north_edge).rev() {
		for east in rect.west_edge..rect.east_edge {
			let pos = Pos { north, east };
			if squad.contains(&pos) {
				print!("#");
			}
			else {
				print!(".");
			}
		}
		println!();
	}
}

/* Task 2 */

fn task2(mut squad: Squad) -> u32 {
	let mut rule_book = init_rule_book();
	let mut prev_moved = 1;
	let mut round_count = 0;
	while prev_moved > 0 {
		prev_moved = elf_round(&mut squad, &mut rule_book);
		round_count += 1;
	}

	return round_count;
}

/* Unit tests */

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..";

    lazy_static! {
        static ref EXAMPLE_SQUAD: Squad = parse_str(EXAMPLE);
    }

    fn parse_str(s: &str) -> Squad {
		parse(io::BufReader::new(s.as_bytes()))
	}

    #[test]
    fn validate_task1() {
        assert_eq!(task1(EXAMPLE_SQUAD.clone()), 110);
    }

    #[test]
    fn validate_task2() {
        assert_eq!(task2(EXAMPLE_SQUAD.clone()), 20);
    }

    #[test]
    fn test_small_steps() {
        let mut squad = parse_str(
".....
..##.
..#..
.....
..##.
.....");
        let mut rule_book = init_rule_book();

        elf_round(&mut squad, &mut rule_book);
        assert_eq!(squad, parse_str(
"..##.
.....
..#..
...#.
..#..
....."));

        elf_round(&mut squad, &mut rule_book);
        assert_eq!(squad, parse_str(
".....
..##.
.#...
....#
.....
..#.."));

        elf_round(&mut squad, &mut rule_book);
        assert_eq!(squad, parse_str(
"..#..
....#
#....
....#
.....
..#.."));
    }

    #[test]
    fn test_example_steps() {
		// == Initial State ==
        let mut squad = parse_str(
"..............
..............
.......#......
.....###.#....
...#...#.#....
....#...##....
...#.###......
...##.#.##....
....#..#......
..............
..............
..............");
        let mut rule_book = init_rule_book();

		// == End of Round 1 ==
        elf_round(&mut squad, &mut rule_book);
        assert_eq!(squad, parse_str(
"..............
.......#......
.....#...#....
...#..#.#.....
.......#..#...
....#.#.##....
..#..#.#......
..#.#.#.##....
..............
....#..#......
..............
.............."));

        // == End of Round 2 ==
        elf_round(&mut squad, &mut rule_book);
        show_squad_within(&squad, &Rect { north_edge: 1, south_edge: -10, east_edge: 12, west_edge: 1 });
        assert_eq!(squad, parse_str(
"..............
.......#......
....#.....#...
...#..#.#.....
.......#...#..
...#..#.#.....
.#...#.#.#....
..............
..#.#.#.##....
....#..#......
..............
.............."));

        // == End of Round 3 ==
        elf_round(&mut squad, &mut rule_book);
        assert_eq!(squad, parse_str(
"..............
.......#......
.....#....#...
..#..#...#....
.......#...#..
...#..#.#.....
.#..#.....#...
.......##.....
..##.#....#...
...#..........
.......#......
.............."));
    }

	#[test]
	fn test_rule_book() {
		let mut rule_book = init_rule_book();

		// Round 1
		let mut iter = rule_book.iter();
		assert_eq!(iter.next(), Some(&North));
		assert_eq!(iter.next(), Some(&South));
		assert_eq!(iter.next(), Some(&West));
		assert_eq!(iter.next(), Some(&East));
		assert_eq!(iter.next(), None);

		// Round 2
		rule_book.advance(1);
		let mut iter = rule_book.iter();
		assert_eq!(iter.next(), Some(&South));
		assert_eq!(iter.next(), Some(&West));
		assert_eq!(iter.next(), Some(&East));
		assert_eq!(iter.next(), Some(&North));
		assert_eq!(iter.next(), None);

		// Round 3
		rule_book.advance(1);
		let mut iter = rule_book.iter();
		assert_eq!(iter.next(), Some(&West));
		assert_eq!(iter.next(), Some(&East));
		assert_eq!(iter.next(), Some(&North));
		assert_eq!(iter.next(), Some(&South));
		assert_eq!(iter.next(), None);
	}
}
