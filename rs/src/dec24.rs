use std::fs::File;
use std::io::{self, BufRead};
use std::cmp::{self, Ordering};
use crate::astar::{astar, AStar};
use std::rc::Rc;
use multimap::MultiMap;
use std::vec;

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

#[derive(PartialEq, Eq, Debug, Hash, Clone, Copy, PartialOrd, Ord)]
struct Pos {
	north: Coord,
	east:  Coord,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Dir {
	North, South, East, West
}
use Dir::*;

impl Dir {
	fn all() -> [Self; 4] {
		[North, South, East, West]
	}
}

impl Pos {
	fn move_in_dir(&self, dir: Dir) -> Self {
		match dir {
			North => Pos { north: self.north + 1, east: self.east },
			South => Pos { north: self.north - 1, east: self.east },
			East  => Pos { north: self.north, east: self.east + 1 },
			West  => Pos { north: self.north, east: self.east - 1 },
		}
	}

	fn manhattan_dist(&self, other: Self) -> Coord {
		let north_diff = self.north - other.north;
		let east_diff = self.east - other.east;
		north_diff.abs() + east_diff.abs()
	}
}

/* Valley */

struct Valley {
	north_wall: Coord,
	south_wall: Coord,
	east_wall:  Coord,
	west_wall:  Coord,

	start_pos:  Pos,
	exit_pos:   Pos,
}

impl Valley {
	fn is_valid_pos(&self, pos: Pos) -> bool {
		let within_box =
			pos.north < self.north_wall
			&& pos.north > self.south_wall
			&& pos.east < self.east_wall
			&& pos.east > self.west_wall;

		within_box
		|| pos == self.start_pos
		|| pos == self.exit_pos
	}
}

/* Expedition */

type Time = u16;
type Blizzards = MultiMap<Pos, Dir>;

#[derive(Clone)]
struct Expedition<'a> {
	curr_pos:  Pos,
	time:      Time,
	blizzards: Rc<Blizzards>,
	valley:    &'a Valley
}

impl<'a> Eq for Expedition<'a> { }

impl<'a> PartialEq for Expedition<'a> {
	fn eq(&self, other: &Self) -> bool {
		self.partial_cmp(other).unwrap() == Ordering::Equal
	}
}

impl<'a> PartialOrd for Expedition<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        /* We can only compare Expeditions in the same Valley */
        let vl1 = self.valley as *const Valley;
        let vl2 = other.valley as *const Valley;
        if vl1 == vl2 {
			/* astar maximizes so we want to minimize */
			// Start by comparing the time + manhattan distance
			// Note that this is safe to do even if the expedition is dead
			match self.potential().cmp(&other.potential()) {
				Ordering::Greater => return Some(Ordering::Less), /* this is worse */
				Ordering::Less    => return Some(Ordering::Greater), /* lower is better */
				Ordering::Equal   => {},
			}
			/* Equal potential: best is with higher time (smaller remaining distance) */
			match self.time.cmp(&other.time) {
				Ordering::Greater => return Some(Ordering::Greater), /* this is somehow better */
				Ordering::Less    => return Some(Ordering::Less), /* higher is better */
				Ordering::Equal   => {},
			}
			/* Now we don't care about ordering, it's just about not being equal */
			match self.curr_pos.cmp(&other.curr_pos) {
				Ordering::Equal   => {},
				neq               => return Some(neq)
			}
			// MultiMap doesn't implement ordering
			// Instead, compare data pointers (could be same if Rc points to the same)
			match Rc::as_ptr(&self.blizzards).cmp(&Rc::as_ptr(&other.blizzards)) {
				Ordering::Equal   => {},
				neq               => return Some(neq)
			}
			return Some(Ordering::Equal);
        }
        else {
            None
        }
    }
}

impl<'a> Ord for Expedition<'a> {
	fn cmp(&self, other: &Self) -> cmp::Ordering {
		self.partial_cmp(other).expect("Can only compare expeditions in the same valley")
	}
}

impl<'a> AStar for Expedition<'a> {
	type Expansion = vec::IntoIter<Self>;

	fn expand(&self) -> Self::Expansion {
		if self.reached_exit() {
			// This tells astar that we're done
			return Vec::new().into_iter()
		}

		let mut post_blizzard: Expedition = self.clone();
		post_blizzard.move_blizzards();
		let candidate_moves = valid_moves(&post_blizzard);
		let next_states =
			if candidate_moves.is_empty() {
				// Fake reaching the end with infinite time
				// WARNING need to make sure we never add anything to that special distance
				vec![expedition_dies(post_blizzard)]
			}
			else {
				// TODO we want the blizzard to be Rc
				// TODO increase time in move_to
				candidate_moves.into_iter()
					.map(|new_pos| {
						let mut moved_xp: Expedition = post_blizzard.clone();
						moved_xp.move_to(new_pos);
						moved_xp
					})
					.collect()
			};
		next_states.into_iter()
	}

	fn prune(&self, curr_best: &Self) -> bool {
		// We want to prune if the potential is higher than the current best time
		self.potential() >= curr_best.time
	}

}

/* Next moves */

fn valid_moves(xp: &Expedition) -> Vec<Pos> {
	let mut candidates = Vec::new();
	for dir in Dir::all() {
		let new_pos = xp.curr_pos.move_in_dir(dir);
		if xp.can_move_to(new_pos) {
			// TODO this needs to test both the edges of the valley and the blizzards
			candidates.push(new_pos);
		}
	}
	// A blizzard may have moved to the current position
	if xp.can_move_to(xp.curr_pos) {
		candidates.push(xp.curr_pos)
	}
	return candidates;
}

fn expedition_dies(mut xp: Expedition) -> Expedition {
	xp.time = Time::MAX;
	xp.curr_pos = xp.valley.exit_pos;
	return xp;
}

impl<'a> Expedition<'a> {
	fn can_move_to(&self, pos: Pos) -> bool {
		// 1. Check map boundaries
		self.valley.is_valid_pos(pos) &&
		// 2. Check blizzards
		!self.blizzards.contains_key(&pos)
	}

	fn move_to(&mut self, new_pos: Pos) {
		self.curr_pos = new_pos;
		assert_ne!(self.time, Time::MAX);
		self.time += 1;
	}

	fn move_blizzards(&mut self) {
		let new_blizzards =
			self.blizzards
				.iter()
				.map(|(pos, dir)| (pos.move_in_dir(*dir), *dir))
				.collect();
		self.blizzards = Rc::new(new_blizzards);
	}

	fn reached_exit(&self) -> bool {
		self.curr_pos == self.valley.exit_pos
	}

	/* In the ideal scenario, in how much total time can we reach the exit? */
	fn potential(&self) -> Time {
		if self.reached_exit() {
			// Note that this also covers the case when the expedition is dead,
			// which is thus safe
			self.time
		}
		else {
			self.time + self.curr_pos.manhattan_dist(self.valley.exit_pos) as u16
		}
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
