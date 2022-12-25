use std::fs::File;
use std::io::{self, BufRead};
use std::collections::HashMap;
use std::ops;

type Input<T> = io::BufReader<T>;

pub fn run(file_content: Input<File>) {
    let parsed = parse(file_content);

    let res1 = task1(&parsed);
    println!("Task 1: {}", res1);
    assert_eq!(res1, 286698846151845);

    //let res2 = task2(&parsed);
    //println!("Task 2: {}", res2);
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
					let op =
						match op_char {
							"+" => ops::Add::add,
							"-" => ops::Sub::sub,
							"*" => ops::Mul::mul,
							// TODO check the remainder
							"/" => ops::Div::div,
							_   => panic!("Unknown operator: {:?}", op_char),
						};

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

/* Monkey eval */

// TODO assert that division is always round
// TODO assert that no overflow
type Value = i64;
type Name = String;
type MOp = fn (Value, Value) -> Value;

enum Monkey {
	Const(Value),
	Op(Name, Name, MOp),
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
				op(monkey_eval(name_left, troop, known),
				   monkey_eval(name_right, troop, known)),
		};

	known.insert(name.clone(), value);
	return value;
}

/* Task 1 */

fn task1(troop: &Troop) -> Value {
	monkey_eval(&String::from("root"), troop, &mut HashMap::new())
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

}
