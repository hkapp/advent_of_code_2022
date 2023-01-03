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
    //let valley = parse(file_content);

    //let res1 = task1(&valley);
    //println!("Task 1: {}", res1);
    //assert_eq!(res1, 245);

    //let res2 = task2(valley);
    //println!("Task 2: {}", res2);
    //assert_eq!(res2, 798);
}

/* Parsing */

//fn parse<T: io::Read>(file_content: Input<T>) -> Valley {
    ///* Fields for Valley */
    //let north_wall = 0;
    //let mut south_wall = None;
    //let mut east_wall = None;
    //let west_wall = 0;
    //let mut start_pos = None;
    //let mut exit_pos = None;

	//let mut blizzards = MultiMap::new();

    //let mut north = north_wall;
    //for line in file_content.lines() {
		//let mut east = west_wall;
		//for c in line.unwrap().chars() {
			//let pos = Pos { north, east };
			//match c {
				//'.' => {
					//if north == north_wall {
						//assert!(start_pos.is_none());
						//start_pos = Some(pos);
					//}
					//else if south_wall.is_some() && south_wall.unwrap() == north {
						//assert!(exit_pos.is_none());
						//exit_pos = Some(pos);
					//}
				//},
				//'#' => {
					//if north == north_wall || east == west_wall {
						///* Do nothing */
					//}
					//else if east == west_wall + 1 {
						//assert!(south_wall.is_none());
						//south_wall = Some(north);
					//}
					//else if east_wall.is_none() {
						//east_wall = Some(east);
					//}
				//},
				//_ => {
					//let dir = c.to_string().parse().unwrap();
					//blizzards.insert(pos, dir);
				//}
			//}
			///* The columns expand eastward */
			//east += 1;
		//}
		///* The lines expand southward */
		//north -= 1;
	//}

	//Valley {
		//north_wall,
		//south_wall: south_wall.unwrap(),
		//east_wall:  east_wall.unwrap(),
		//west_wall,

		//start_pos: start_pos.unwrap(),
		//exit_pos:  exit_pos.unwrap(),

		//blizzard_history: RefCell::new(
							//HashMap::from([(0, blizzards)]),)
	//}
//}

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

    #[test]
    fn validate_task2() {
        assert_eq!(task2(example_valley()), 54);
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

	#[test]
	fn test_compare() {
		let valley = example_valley();

		let xp1 = Expedition::new(&valley);

		let mut xp2 = Expedition::new(&valley);
		xp2.time += 1;
		// xp2 is worse than xp1 (waited in place)
		assert!(xp1 > xp2);

		xp2.move_to(xp2.curr_pos.move_in_dir(South));
		// xp2 is better than xp1 (it actually moved towards the goal)
		assert!(xp2 > xp1);
	}

}
