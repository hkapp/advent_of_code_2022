use std::fs::File;
use std::io::{self, BufRead};
use std::cmp::{self, Ordering};
use crate::astar::{astar, AStar};
use std::cell::{self, RefCell};
use std::collections::HashMap;
use multimap::MultiMap;
use std::vec;
use std::slice;
use std::str::FromStr;
use std::hash::Hash;

type Input<T> = io::BufReader<T>;

pub fn run(file_content: Input<File>) {
    let valley = parse(file_content);

    let res1 = task1(&valley);
    println!("Task 1: {}", res1);
    //assert_eq!(res1, 4056);

    //let res2 = task2(squad);
    //println!("Task 2: {}", res2);
    //assert_eq!(res2, 999);
}

/* Parsing */

fn parse<T: io::Read>(file_content: Input<T>) -> Valley {
    /* Fields for Valley */
    let north_wall = 0;
    let mut south_wall = None;
    let mut east_wall = None;
    let west_wall = 0;
    let mut start_pos = None;
    let mut exit_pos = None;

	let mut blizzards = MultiMap::new();

    let mut north = north_wall;
    for line in file_content.lines() {
		let mut east = west_wall;
		for c in line.unwrap().chars() {
			let pos = Pos { north, east };
			match c {
				'.' => {
					if north == north_wall {
						assert!(start_pos.is_none());
						start_pos = Some(pos);
					}
					else if south_wall.is_some() && south_wall.unwrap() == north {
						assert!(exit_pos.is_none());
						exit_pos = Some(pos);
					}
				},
				'#' => {
					if north == north_wall || east == west_wall {
						/* Do nothing */
					}
					else if east == west_wall + 1 {
						assert!(south_wall.is_none());
						south_wall = Some(north);
					}
					else if east_wall.is_none() {
						east_wall = Some(east);
					}
				},
				_ => {
					let dir = c.to_string().parse().unwrap();
					blizzards.insert(pos, dir);
				}
			}
			/* The columns expand eastward */
			east += 1;
		}
		/* The lines expand southward */
		north -= 1;
	}

	Valley {
		north_wall,
		south_wall: south_wall.unwrap(),
		east_wall:  east_wall.unwrap(),
		west_wall,

		start_pos: start_pos.unwrap(),
		exit_pos:  exit_pos.unwrap(),

		blizzard_history: RefCell::new(
							HashMap::from([(0, blizzards)]),)
	}
}

impl FromStr for Dir {
	type Err = String;

	fn from_str(s: &str) -> Result<Self, <Self as FromStr>::Err> {
		match s {
			">" => Ok(East),
			"<" => Ok(West),
			"^" => Ok(North),
			"v" => Ok(South),
			_   => Err(format!("Unrecognized direction: {:?}", s)),
		}
	}
}

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

	blizzard_history: RefCell<HashMap<Time, Blizzards>>,
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

	fn blizzard_at_time<'a >(&'a self, t: Time) -> cell::Ref<'a, Blizzards> {
		if let Some(b) = self.get_cached_blizzard(t) {
			b
		}
		else {
			let prev_blizzard = self.get_cached_blizzard(t-1)
								.expect("No previous blizzard to compute from");
			let mut new_blizzard = prev_blizzard.clone();
			drop(prev_blizzard);
			move_blizzards(&mut new_blizzard, self);
			self.blizzard_history.borrow_mut().insert(t, new_blizzard);
			self.get_cached_blizzard(t).unwrap()
		}
	}

	fn get_cached_blizzard(&self, t: Time) -> Option<cell::Ref<Blizzards>> {
		cell::Ref::filter_map(
			self.blizzard_history.borrow(),
			|hist| hist.get(&t))
			.ok()
	}
}

/* Blizzards */

type Blizzards = MultiMap<Pos, Dir>;

fn move_blizzards(blizzards: &mut Blizzards, valley: &Valley) {
	let move_one_blizzard = |curr_pos: Pos, dir| {
		let new_pos = curr_pos.move_in_dir(dir);
		if valley.is_valid_pos(new_pos) {
			new_pos
		}
		else {
			// Wrap around
			/*  +--^--v--+
			 *  >        >
			 *  |        |
			 *  <        <
			 *  +--^--v--+
			 */
			match dir {
				North => Pos { north: valley.south_wall + 1, east: new_pos.east },
				South => Pos { north: valley.north_wall - 1, east: new_pos.east },
				East  => Pos { north: new_pos.north, east: valley.west_wall + 1 },
				West  => Pos { north: new_pos.north, east: valley.east_wall - 1 },
			}
		}
	};

	let prev_size = multi_map_len(&blizzards);
	let new_blizzards: Blizzards =
		multi_iter(&blizzards)
			.map(|(pos, dir)| (move_one_blizzard(*pos, *dir), *dir))
			.collect();
	assert_eq!(multi_map_len(&new_blizzards), prev_size, "new_blizzards: {:?}", new_blizzards);

	*blizzards = new_blizzards;
}

// For some reason, this is not part of the multimap crate
fn multi_iter<'a, K: Eq + Hash, V>(multi_map: &'a MultiMap<K, V>) -> MultiIter<'a, K, V> {
	MultiIter::new(multi_map)
}

struct MultiIter<'a, K, V> {
	curr_iter:  Option<(&'a K, slice::Iter<'a, V>)>,
	vec_iter:   multimap::IterAll<'a, K, Vec<V>>,
}

impl<'a, K, V> Iterator for MultiIter<'a, K, V> {
	type Item = (&'a K, &'a V);

	fn next(&mut self) -> Option<Self::Item> {
		match &mut self.curr_iter {
			Some((curr_key, value_iter)) => match value_iter.next() {
				Some(curr_value) => Some((curr_key, curr_value)),
				None             => {
					self.next_vec();
					self.next()
				}
			}
			None => None,
		}
	}
}

impl<'a, K, V> MultiIter<'a, K, V> {
	fn next_vec(&mut self) {
		self.curr_iter =
			self.vec_iter
				.next()
				.map(|(k, vec)| (k, vec.iter()))
	}
}

impl<'a, K: Eq + std::hash::Hash, V> MultiIter<'a, K, V> {
	fn new(multi_map: &'a MultiMap<K, V>) -> Self {
		let mut new = MultiIter {
			vec_iter:  multi_map.iter_all(),
			curr_iter: None,
		};
		new.next_vec();
		new
	}
}

// For some reason, this is also part of the multimap crate
fn multi_map_len<K: Eq + Hash, V>(multi_map: &MultiMap<K, V>) -> usize {
	multi_map.iter_all()
		.map(|(_, vec)| vec.len())
		.sum()
}

/* Expedition */

type Time = u16;

#[derive(Clone)]
struct Expedition<'a> {
	curr_pos:  Pos,
	time:      Time,
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
			/* astar maximizes but we want to minimize */
			// It's always better to escape
			if self.reached_exit() && !other.reached_exit() {
				return Some(Ordering::Greater);
			}
			if !self.reached_exit() && other.reached_exit() {
				return Some(Ordering::Less);
			}
			// Now compare the time + manhattan distance
			// Note that this is safe to do even if the expedition is dead
			// Also note that if we only compare distance to exit,
			// we do greedy search which might end up in a non-working loop
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
			println!("\nReached the exit in {} moves", self.time);
			return Vec::new().into_iter();
		}

		let mut post_blizzard: Expedition = self.clone();
		//post_blizzard.move_blizzards();
		// Note: modifying the time is now enough to move the blizzard
		post_blizzard.time += 1;
		let candidate_moves = valid_moves(&post_blizzard);
		let next_states =
			if candidate_moves.is_empty() {
				// Fake reaching the end with infinite time
				// WARNING need to make sure we never add anything to that special distance
				vec![expedition_dies(post_blizzard)]
			}
			else {
				candidate_moves.into_iter()
					.map(|new_pos| {
						let mut moved_xp: Expedition = post_blizzard.clone();
						moved_xp.move_to(new_pos);
						//println!("{}", moved_xp.potential());
						moved_xp
					})
					.collect()
			};
		next_states.into_iter()
	}

	fn prune(&self, curr_best: &Self) -> bool {
		//println!("Curr best: {}exit {}, potential: {}",
			//if curr_best.reached_exit() { "" } else { "no " }, curr_best.time, self.potential());
		//if curr_best.reached_exit()
			//&& self.potential() >= curr_best.time
		//{
			//print!("-");
		//}
		// We want to prune if the potential is higher than the current best time
		// Only if the curr_best reached the end though
		curr_best.reached_exit()
		&& self.potential() >= curr_best.time
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
		!self.blizzards().contains_key(&pos)
	}

	fn move_to(&mut self, new_pos: Pos) {
		// Note: the time is now moved when we move the blizzard
		self.curr_pos = new_pos;
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

	fn new(valley: &'a Valley) -> Self {
		Expedition {
			curr_pos:  valley.start_pos,
			time:      0,
			valley,
		}
	}

	fn blizzards(&self) -> cell::Ref<'_, Blizzards> {
		self.valley.blizzard_at_time(self.time)
	}
}

/* Task 1 */

fn task1(valley: &Valley) -> Time {
	let /*mut*/ start_xp = Expedition::new(valley);
	// TODO remove
	//let start_pos = Pos {
		//north: valley.south_wall + 1,
		//east:  valley.east_wall - 3,
	//};
	//start_xp.curr_pos = start_pos;

	let final_xp = astar(start_xp);
	final_xp.time
}

/* Unit tests */

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#";

    //lazy_static! {
        //static ref EXAMPLE_VALLEY:    Valley = parse_str(EXAMPLE);
    //}
	fn example_valley() -> Valley {
		parse_str(EXAMPLE)
	}

    fn parse_str(s: &str) -> Valley {
		parse(io::BufReader::new(s.as_bytes()))
	}

    #[test]
    fn validate_task1() {
        assert_eq!(task1(&example_valley()), 18);
    }

	fn assert_fmt<T: Eq + std::fmt::Debug + std::fmt::Display>(left: T, right: T) {
		assert_eq!(left, right, "\nleft:\n{}\nright:\n{}\n", left, right);
	}

    #[test]
    fn validate_task1_steps() {
		let valley = example_valley();
		let mut xp = Expedition::new(&valley);
		let expected = "#E######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#";
		assert_fmt::<&str>(&show_expedition(&xp), expected);

		/* Minute 1, move down: */
		xp.move_to(xp.curr_pos.move_in_dir(South));
		xp.time += 1;
		let expected = "#.######
#E>3.<.#
#<..<<.#
#>2.22.#
#>v..^<#
######.#";
		assert_fmt::<&str>(&show_expedition(&xp), expected);

		/* Minute 2, move down: */
		xp.move_to(xp.curr_pos.move_in_dir(South));
		xp.time += 1;
		let expected = "#.######
#.2>2..#
#E^22^<#
#.>2.^>#
#.>..<.#
######.#";
		assert_fmt::<&str>(&show_expedition(&xp), expected);
	}

	impl Into<char> for Dir {
		fn into(self) -> char {
			match self {
				North => '^',
				South => 'v',
				East  => '>',
				West  => '<',
			}
		}
	}

	fn show_expedition(xp: &Expedition) -> String {
		let mut buf = String::new();
		for north in (xp.valley.south_wall..(xp.valley.north_wall+1)).rev() {
			for east in xp.valley.west_wall..(xp.valley.east_wall+1) {
				let pos = Pos { north, east };
				let c =
					if xp.valley.is_valid_pos(pos) {
						if xp.curr_pos == pos {
							'E'
						}
						else {
							let empty_vec = Vec::new();
							let curr_blizzards = xp.blizzards();
							let blizzards_here = curr_blizzards.get_vec(&pos).unwrap_or(&empty_vec);
							match blizzards_here.len() {
								0 => '.',
								1 => (*blizzards_here.get(0).unwrap()).into(),
								n => {
									assert!(n < 10);
									format!("{}", n).chars().next().unwrap()
								}
							}
						}
					}
					else {
						// This must be a wall
						'#'
					};
				buf.push(c);
			}
			if north > xp.valley.south_wall {
				buf.push('\n');
			}
		}
		return buf;
	}

    #[test]
    fn test_blizzard_wraparound() {
		let simpler_example: &str = "#.######
#....^>#
#......#
#<.....#
#....v.#
######.#";
		let valley = parse_str(simpler_example);
		let mut xp = Expedition::new(&valley);
		let expected = "#E######
#....^>#
#......#
#<.....#
#....v.#
######.#";
		assert_fmt::<&str>(&show_expedition(&xp), expected);

		xp.time += 1;
		let expected = "#E######
#>...v.#
#......#
#.....<#
#....^.#
######.#";
		assert_fmt::<&str>(&show_expedition(&xp), expected);
	}

	#[test]
	fn test_multi_iter() {
		let mut multi_map = MultiMap::new();
		multi_map.insert(2 as u8, 2 as u8);
		multi_map.insert(3 as u8, 3 as u8);
		multi_map.insert(4 as u8, 4 as u8);
		multi_map.insert(4 as u8, 2 as u8);

		assert_eq!(multi_map.iter().count(), 3);
		assert_eq!(multi_map.iter_all().count(), 3);
		assert_eq!(multi_iter(&multi_map).count(), 4);
	}

	#[test]
	fn test_multi_map_collect() {
		let mut multi_map = MultiMap::new();
		multi_map.insert(2 as u8, 2 as u8);
		multi_map.insert(3 as u8, 3 as u8);
		multi_map.insert(4 as u8, 4 as u8);
		multi_map.insert(4 as u8, 2 as u8);

		let new_map: MultiMap<u8, u8> =
			multi_iter(&multi_map)
				.map(|(k, v)| (*k, *v))
				.collect();

		assert_eq!(new_map, multi_map);
	}

	#[test]
	fn test_multi_map_len() {
		let mut multi_map = MultiMap::new();
		multi_map.insert(2 as u8, 2 as u8);
		multi_map.insert(3 as u8, 3 as u8);
		multi_map.insert(4 as u8, 4 as u8);
		multi_map.insert(4 as u8, 2 as u8);

		assert_eq!(multi_map.len(), 3);
		assert_eq!(multi_map_len(&multi_map), 4);
	}

	#[test]
	fn test_blizzard_reccurrence() {
		let valley = example_valley();
		let mut xp = Expedition::new(&valley);
		let mut srep = show_expedition(&xp);
		let mut archive = HashMap::new();
		let mut n = 0;

		while !archive.contains_key(&srep) {
			archive.insert(srep, n);
			xp.time += 1;
			srep = show_expedition(&xp);
			n += 1;
		}

		let north_recurr = xp.valley.north_wall - xp.valley.south_wall - 1;
		let east_recurr = xp.valley.east_wall - xp.valley.west_wall - 1;
		// We should use GCD here but it's not in the standard lib
		// see https://users.rust-lang.org/t/why-no-gcd-in-standard-lib/36490
		let max_recurr = north_recurr * east_recurr;
		assert_eq!(max_recurr % n, 0);
	}

	#[test]
	fn test_compare_death() {
		let valley = example_valley();

		let mut xp1 = Expedition::new(&valley);
		xp1 = expedition_dies(xp1);

		let mut xp2 = Expedition::new(&valley);

		// xp1 > xp2 because xp1 reaches the end
		assert!(xp1 > xp2);

		xp2.curr_pos = valley.exit_pos;
		// xp2 > xp1 because both exited but xp2.time < xp1.time
		assert!(xp2 > xp1);
	}

}
