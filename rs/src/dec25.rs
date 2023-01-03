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

impl FromStr for Snafu {
	type Err = String;

	fn from_str(s: &str) -> Result<Self, <Self as FromStr>::Err> {
		fn parse_digit(c: char) -> Result<Digit, String> {
			match c {
				'2' => Ok(2),
				'1' => Ok(1),
				'0' => Ok(0),
				'-' => Ok(-1),
				'=' => Ok(-2),
				_   => Err(format!("Invalid digit: {:?}", c)),
			}
		}

		s.chars()
			.map(parse_digit)
			.rev()
			.collect::<Result<Vec<Digit>, Self::Err>>()
			.map(|digits| Snafu { digits } )
	}
}

/* Snafu */

type Digit = i8;

struct Snafu {
	digits: Vec<Digit>
}

type Decimal = i32;

impl Snafu {
	fn as_decimal(&self) -> Decimal {
		self.digits
			.iter()
			.fold((1, 0 as Decimal),
				|(pow, sum), dig| (pow*5, sum + *dig as Decimal * pow))
			.1
	}
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

    //fn parse_str(s: &str) -> Valley {
		//parse(io::BufReader::new(s.as_bytes()))
	//}

    #[test]
    fn test_as_decimal() {
		fn test_one(s: &str, n: Decimal) {
			assert_eq!(s.parse::<Snafu>().unwrap().as_decimal(), n);
		}

        test_one("1", 1);
        test_one("2", 2);
        test_one("1=", 3);
		test_one("1-",          4  );
		test_one("10",          5  );
		test_one("11",          6  );
		test_one("12",          7  );
		test_one("2=",          8  );
		test_one("2-",          9  );
		test_one("20",         10  );
		test_one("1=0",         15  );
		test_one("1-0",         20  );
		test_one("1=11-2",       2022  );
		test_one("1-0---0",      12345  );
		test_one("1121-1110-1=0",  314159265);
    }


}
