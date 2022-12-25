use std::fs::File;
use std::io::{self, BufRead};
use std::collections::HashMap;
use std::ops;
use std::str::FromStr;

type Input<T> = io::BufReader<T>;

pub fn run(file_content: Input<File>) {
    let parsed = parse(file_content);

    let res1 = task1(&parsed);
    println!("Task 1: {}", res1);
    assert_eq!(res1, 286698846151845);

    let res2 = task2(&parsed);
    println!("Task 2: {}", res2);
    //assert_eq!(res2, 62744);
}

/* Parsing */

fn parse<T: io::Read>(file_content: Input<T>) -> Troop {
	fn parse_monkey(s: String) -> (Name, Monkey) {
		let mut elem_iter = s.split_whitespace();

		let tmp_name = elem_iter.next().unwrap();
		let name = String::from(&tmp_name[0..4]);
		assert_eq!(name.len(), 4);

		let decider = elem_iter.next().unwrap();
		let monkey =
			match decider.parse::<Value>() {
				Ok(n) => {
					Monkey::Const(n)
				}
				Err(_) => {
					let op_char = elem_iter.next().unwrap();
					assert_eq!(op_char.len(), 1);
					let op = op_char.parse().unwrap();

					let left_name = String::from(decider);
					assert_eq!(left_name.len(), 4);

					let tmp_right_name = elem_iter.next().unwrap();
					let right_name = String::from(tmp_right_name);
					assert_eq!(right_name.len(), 4);

					Monkey::Op(left_name, right_name, op)
				}
			};
		assert_eq!(elem_iter.next(), None);

		return (name, monkey);
	}

    file_content
        .lines()
        .map(io::Result::unwrap)
        .map(parse_monkey)
        .collect::<Troop>()
}

impl FromStr for BinOp {
    type Err = String;

    fn from_str(op_char: &str) -> Result<Self, Self::Err> {
		match op_char {
			"+" => Ok(BinOp::Add),
			"-" => Ok(BinOp::Sub),
			"*" => Ok(BinOp::Mul),
			"/" => Ok(BinOp::Div),
			_   => Err(format!("Unknown operator: {:?}", op_char)),
		}
	}
}

/* BinOp */

#[derive(Clone, Copy)]
enum BinOp {
	Add,
	Sub,
	Mul,
	Div
}

impl BinOp {
	fn reciprocal(&self) -> Self {
		use BinOp::*;
		match self {
			Add => Sub,
			Sub => Add,
			Mul => Div,
			Div => Mul,
		}
	}

	fn apply(&self, x: Value, y: Value) -> Value {
		use BinOp::*;
		match self {
			Add => x + y,
			Sub => x - y,
			Mul => x * y,
			Div => x / y,
		}
	}
}

/* Monkey eval */

// TODO assert that division is always round
// TODO assert that no overflow
type Value = i64;
type Name = String;

enum Monkey {
	Const(Value),
	Op(Name, Name, BinOp),
}

type Troop = HashMap<Name, Monkey>;

fn monkey_eval(name: &Name, troop: &Troop, known: &mut HashMap<Name, Value>)
	-> Value
{
	if let Some(value) = known.get(name) {
		return *value;
	}

	let monkey = troop.get(name).expect("Couldn't find monkey");

	let value =
		match monkey {
			Monkey::Const(n) => *n,
			Monkey::Op(name_left, name_right, op) =>
				op.apply(
					monkey_eval(name_left, troop, known),
					monkey_eval(name_right, troop, known)),
		};

	known.insert(name.clone(), value);
	return value;
}

/* Task 1 */

fn task1(troop: &Troop) -> Value {
	monkey_eval(&String::from("root"), troop, &mut HashMap::new())
}

/* Task 2 */

fn print_paths_to(curr_path: &[Name], dest: &Name, troop: &Troop) {
	let curr_name = curr_path.last().unwrap();
	if curr_name == dest {
		println!("{}", curr_path.join(" -> "));
		return;
	}

	match troop.get(curr_name).unwrap() {
		Monkey::Const(_) => {},
		Monkey::Op(left_name, right_name, _) => {
			let mut left_path = Vec::from(curr_path);
			left_path.push(left_name.clone());
			print_paths_to(&left_path, dest, troop);

			let mut right_path = left_path;
			right_path.pop();
			right_path.push(right_name.clone());
			print_paths_to(&right_path, dest, troop);
		}
	}
}

fn count_paths_to(curr_name: &Name, dest: &Name, troop: &Troop) -> u16 {
	if curr_name == dest {
		return 1;
	}

	match troop.get(curr_name).unwrap() {
		Monkey::Const(_) => 0,
		Monkey::Op(left_name, right_name, _) => {
			let left = count_paths_to(&left_name, dest, troop);
			let right = count_paths_to(&right_name, dest, troop);
			left + right
		}
	}
}

type Solver = Vec<EqStep>;
struct EqStep {
	left:  Option<Value>,
	right: Option<Value>,
	op:    BinOp
}

fn extend_solver_left(left_val: Value, op: BinOp, mut solver: Solver) -> Solver {
	use BinOp::*;
	let a = Some(left_val);
	let b = None;
	let new_step =
		match op {
			/* a + x = b <=> x = b - a */
			Add => EqStep {
				left:  b,
				op:    Sub,
				right: a,
			},
			/* a - x = b <=> x = a - b */
			Sub => EqStep {
				left:  a,
				op:    Sub,
				right: b
			},
			/* a * x = b <=> x = b / a */
			Mul => EqStep {
				left:  b,
				op:    Div,
				right: a
			},
			/* a / x = b <=> x = a / b */
			Div => EqStep {
				left:  a,
				op:    Div,
				right: b
			}
		};

	solver.push(new_step);
	return solver;
}

/* x + a = b <=> x = b - a
 * x - a = b <=> x = b + a
 * x * a = b <=> x = b / a
 * x / a = b <=> x = b * a
 */
fn extend_solver_right(mut solver: Solver, op: BinOp, right_val: Value) -> Solver {
	solver.push(EqStep {
		left:  None,
		right: Some(right_val),
		op:    op.reciprocal(),
	});
	return solver;
}

fn build_equation(curr_node: &Name, troop: &Troop) -> Result<Value, Solver> {
	if curr_node == &String::from("humn") {
		return Err(Vec::new());
	}

	let curr_monkey = troop.get(curr_node).unwrap();
	match curr_monkey {
		Monkey::Const(n) => Ok(*n),
		Monkey::Op(left_name, right_name, op) => {
			let left_res = build_equation(left_name, troop);
			let right_res = build_equation(right_name, troop);

			match (left_res, right_res) {
				(Ok(vl), Ok(vr)) => Ok(op.apply(vl, vr)),

				(Ok(vl), Err(eqr)) => Err(extend_solver_left(vl, *op, eqr)),

				(Err(eql), Ok(vr)) => Err(extend_solver_right(eql, *op, vr)),

				(Err(_), Err(_)) => panic!("Can't have both sides return equations"),
			}
		}
	}
}

fn task2(troop: &Troop) -> Value {
	let root_name = String::from("root");
	assert_eq!(
		count_paths_to(&root_name, &String::from("humn"), troop),
		1);

	let root = troop.get(&root_name).unwrap();
	let (left_name, right_name) =
		match root {
			Monkey::Const(_) => panic!("Root monkey can't be const"),
			Monkey::Op(left_name, right_name, _) => (left_name, right_name),
		};

	let lhs = build_equation(left_name, troop);
	let rhs = build_equation(right_name, troop);

	/* Now solve the equation */
	let (eq_side, val_side) =
		match (lhs, rhs) {
			(Ok(val), Err(solver)) => (solver, val),
			(Err(solver), Ok(val)) => (solver, val),
			_  => panic!("Can't have both sides be equations or both be values"),
		};

	let mut curr_value = val_side;
	let mut solver = eq_side;
	for eq_step in solver.into_iter() {
		let lstep = eq_step.left.unwrap_or(curr_value);
		let rstep = eq_step.right.unwrap_or(curr_value);
		curr_value = eq_step.op.apply(lstep, rstep);
	}

	return curr_value;
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
        static ref EXAMPLE_PARSED: Troop =
            parse(io::BufReader::new(EXAMPLE.as_bytes()));
    }

    #[test]
    fn validate_task1() {
        assert_eq!(task1(&EXAMPLE_PARSED), 152);
    }

    #[test]
    fn validate_task2() {
        assert_eq!(task2(&EXAMPLE_PARSED), 301);
    }

}
