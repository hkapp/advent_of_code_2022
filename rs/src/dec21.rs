use std::fs::File;
use std::io::{self, BufRead};
use std::collections::HashMap;
use std::ops;

type Input<T> = io::BufReader<T>;

pub fn run(file_content: Input<File>) {
    let parsed = parse(file_content);

    //let res1 = task1(&parsed);
    //println!("Task 1: {}", res1);
    //assert_eq!(res1, 8302);

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

/* Unit tests */

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "1
2
-3
3
-2
0
4";

    lazy_static! {
        static ref EXAMPLE_PARSED: Vec<Val> =
            parse(io::BufReader::new(EXAMPLE.as_bytes()));
    }

    #[test]
    fn validate_task1() {
        assert_eq!(task1(&EXAMPLE_PARSED), 3);
    }

	fn align_heads(to_align: &mut Vec<Shift>, reference: &[Shift]) {
		let anchor = reference[0];
		let anchor_pos = find_pos(&to_align, |x| *x == anchor).expect("Couldn't find anchor");
		to_align.rotate_left(anchor_pos);
	}

    #[test]
    fn validate_task1_steps() {
        let mut message = Vec::from(&EXAMPLE_PARSED as &[Val]);

        let mut step = |shift, expected| {
			let id_usz = find_shift_pos(&EXAMPLE_PARSED, shift).expect("Couldn't find shift value");
			let val = Val { id: id_usz as Id, shift };
			shuffle_once(&mut message, val);
			let mut message_shifts: Vec<Shift> = message.iter().map(|x| x.shift).collect();
			align_heads(&mut message_shifts, expected);
			assert_eq!(&message_shifts, expected);
		};

        step(1, &[2, 1, -3, 3, -2, 0, 4]);
        step(2, &[1, -3, 2, 3, -2, 0, 4]);
        step(-3, &[1, 2, 3, -2, -3, 0, 4]);
        step(3, &[1, 2, -2, -3, 0, 3, 4]);
        step(-2, &[1, 2, -3, 0, 3, 4, -2]);
        step(0, &[1, 2, -3, 0, 3, 4, -2]);
        step(4, &[1, 2, -3, 4, 0, 3, -2]);
    }

    #[test]
    fn validate_task2() {
        assert_eq!(task2(&EXAMPLE_PARSED), 1623178306);
    }

}
