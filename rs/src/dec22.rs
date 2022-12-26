use std::fs::File;
use std::io::{self, BufRead};
use std::collections::HashMap;
use std::ops;
use std::str::FromStr;

type Input<T> = io::BufReader<T>;

pub fn run(file_content: Input<File>) {
    //let parsed = parse(file_content);

    //let res1 = task1(&parsed);
    //println!("Task 1: {}", res1);
    //assert_eq!(res1, 286698846151845);

    //let res2 = task2(&parsed);
    //println!("Task 2: {}", res2);
    //assert_eq!(res2, 62744);
}

/* Parsing */

//fn parse<T: io::Read>(file_content: Input<T>) -> Troop {
	//fn parse_monkey(s: String) -> (Name, Monkey) {
		//let mut elem_iter = s.split_whitespace();

		//let tmp_name = elem_iter.next().unwrap();
		//let name = String::from(&tmp_name[0..4]);
		//assert_eq!(name.len(), 4);

		//let decider = elem_iter.next().unwrap();
		//let monkey =
			//match decider.parse::<Value>() {
				//Ok(n) => {
					//Monkey::Const(n)
				//}
				//Err(_) => {
					//let op_char = elem_iter.next().unwrap();
					//assert_eq!(op_char.len(), 1);
					//let op = op_char.parse().unwrap();

					//let left_name = String::from(decider);
					//assert_eq!(left_name.len(), 4);

					//let tmp_right_name = elem_iter.next().unwrap();
					//let right_name = String::from(tmp_right_name);
					//assert_eq!(right_name.len(), 4);

					//Monkey::Op(left_name, right_name, op)
				//}
			//};
		//assert_eq!(elem_iter.next(), None);

		//return (name, monkey);
	//}

    //file_content
        //.lines()
        //.map(io::Result::unwrap)
        //.map(parse_monkey)
        //.collect::<Troop>()
//}

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
		match dir {
			Left  => {
				assert_eq!(pos.column(), 0);
				/* New column is the maximum column for this row */
				let new_col = self.max_col_on_row(pos.row());
				pos.with_column(new_col)
			}
			Right => {
				let new_col = self.min_col_on_row(pos.row());
				pos.with_column(new_col)
			}
			Up => {
				assert_eq!(pos.row(), 0);
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
	fn max_col_on_row(&self, rowIdx: Coord) -> Coord {
		self.compute_wraparound(rowIdx, true, true)
	}

	fn min_col_on_row(&self, rowIdx: Coord) -> Coord {
		self.compute_wraparound(rowIdx, true, false)
	}

	fn max_row_on_col(&self, colIdx: Coord) -> Coord {
		self.compute_wraparound(colIdx, false, true)
	}

	fn min_row_on_col(&self, colIdx: Coord) -> Coord {
		self.compute_wraparound(colIdx, false, false)
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

	fn turn(&mut self, new_dir: Dir) {
		self.curr_dir = new_dir;
	}
}

/* Task 1 */

enum Move {
	Walk(Coord),
	Turn(Dir)
}

fn init_pos(planet: &Planet) -> Pos {
	let row = 1;
	let col = planet.min_col_on_row(row);
	Pos::from_row_col(row, col)
}

const INIT_DIR: Dir = Dir::Right;

fn task1(planet: &Planet, moves: &[Move]) {
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

	// TODO compute final result
}

/* Unit tests */

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32";

    lazy_static! {
        static ref EXAMPLE_PARSED: Troop = parse_test(EXAMPLE);
    }

    fn parse_test(s: &str) -> Troop {
		parse(io::BufReader::new(s.as_bytes()))
	}

    #[test]
    fn validate_task1() {
        assert_eq!(task1(&EXAMPLE_PARSED), 152);
    }

    #[test]
    fn validate_task2() {
        assert_eq!(task2(&EXAMPLE_PARSED), 301);
    }

	fn build_troop_eq1(
		left: Option<Value>, op: BinOp, right: Option<Value>,
		res: Value)
		-> Troop
    {
		let mut troop = Troop::new();

		let a_name = String::from("aaaa");
		let b_name = String::from("bbbb");
		let root = Monkey::Op(a_name.clone(), b_name.clone(), BinOp::Add);
		troop.insert(String::from("root"), root);

		let b = Monkey::Const(res);
		troop.insert(b_name, b);

		let humn_name = String::from("humn");
		let c_name = String::from("cccc");

		let a_left =
			match left {
				None    => humn_name.clone(),
				Some(_) => c_name.clone(),
			};

		let a_right =
			match right {
				None    => humn_name.clone(),
				Some(_) => c_name.clone(),
			};

		let a = Monkey::Op(a_left, a_right, op);
		troop.insert(a_name, a);

		let c = Monkey::Const(left.or(right).unwrap());
		troop.insert(c_name, c);

		let humn = Monkey::Const(0);
		troop.insert(humn_name, humn);

		return troop;
	}

    #[test]
    fn test_eq_solver1() {
		/* x + 3 = 5 */
		let troop = build_troop_eq1(None, BinOp::Add, Some(3), 5);
		/* x = 2 */
		assert_eq!(task2(&troop), 2);
    }

    #[test]
    fn test_eq_solver2() {
		/* x * 2 = 8 */
		let troop = build_troop_eq1(None, BinOp::Mul, Some(2), 8);
		/* x = 4 */
		assert_eq!(task2(&troop), 4);
    }

    #[test]
    fn test_eq_solver3() {
		/* 7 + x = 8 */
		let troop = build_troop_eq1(Some(7), BinOp::Add, None, 8);
		/* x = 1 */
		assert_eq!(task2(&troop), 1);
    }

    #[test]
    fn test_eq_solver4() {
		/* 7 - x = 5 */
		let troop = build_troop_eq1(Some(7), BinOp::Sub, None, 5);
		/* x = 2 */
		assert_eq!(task2(&troop), 2);
    }

    #[test]
    fn test_eq_solver5() {
		/* 7 * x = 35 */
		let troop = build_troop_eq1(Some(7), BinOp::Mul, None, 35);
		/* x = 5 */
		assert_eq!(task2(&troop), 5);
    }

    #[test]
    fn test_eq_solver6() {
		/* 35 / x = 7 */
		let troop = build_troop_eq1(Some(35), BinOp::Div, None, 7);
		/* x = 5 */
		assert_eq!(task2(&troop), 5);
    }

}
