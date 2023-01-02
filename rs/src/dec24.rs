use std::fs::File;
use std::io::{self, BufRead};
use std::collections::{HashMap, HashSet};
use std::slice;
use std::mem;

type Input<T> = io::BufReader<T>;

pub fn run(file_content: Input<File>) {
    //let squad = parse(file_content);

    //let res1 = task1(squad.clone());
    //println!("Task 1: {}", res1);
    //assert_eq!(res1, 4056);

    //let res2 = task2(squad);
    //println!("Task 2: {}", res2);
    //assert_eq!(res2, 999);
}

/* Parsing */

//fn parse<T: io::Read>(file_content: Input<T>) -> Squad {
    //let mut squad = Squad::new();
    //let mut north = 0;
    //for line in file_content.lines() {
		//let mut east = 0;
		//for c in line.unwrap().chars() {
			//match c {
				//'.' => {},
				//'#' => {
					//let pos = Pos { north, east };
					//squad.insert(pos);
				//},
				//_   => panic!("Unrecognized input character: {:?}", c),
			//}
			///* The columns expand eastward */
			//east += 1;
		//}
		///* The lines expand southward */
		//north -= 1;
	//}
	//return squad;
//}

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
