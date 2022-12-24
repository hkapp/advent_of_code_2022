use std::fs::File;
use std::io::{self, BufRead};

type Input<T> = io::BufReader<T>;

pub fn run(file_content: Input<File>) {
    let parsed = parse(file_content);

    let res1 = task1(&parsed);
    println!("Task 1: {}", res1);
    assert_eq!(res1, 8302);

    let res2 = task2(&parsed);
    println!("Task 2: {}", res2);
    //assert_eq!(res2, 62744);
}

/* Parsing */

fn parse<T: io::Read>(file_content: Input<T>) -> Vec<Val> {
    file_content
        .lines()
        .map(io::Result::unwrap)
        .map(|s| s.parse())
        .map(Result::unwrap)
        .enumerate()
        .map(Val::from_pair)
        .collect::<Vec<Val>>()
}

/* Shuffle */

type Id    = u16;
type Shift = i16;

#[derive(Eq, PartialEq, Debug, Clone, Copy)]
struct Val {
	id:    Id,
	shift: Shift
}

impl Val {
	fn from_pair(pair: (usize, Shift)) -> Self {
		match pair {
			(id_usz, shift) => Val {
				id: id_usz as Id,
				shift
			}
		}
	}
}

fn find_pos<T, F>(values: &[T], pred: F) -> Option<usize>
	where
		F: Fn(&T) -> bool
{
	values.iter()
		.enumerate()
		.find(|(_idx, val)| pred(val))
		.map(|(idx, _val)| idx)
}

fn find_id_pos(values: &[Val], id: Id) -> Option<usize> {
	find_pos(values, |val| val.id == id)
}

fn find_shift_pos(values: &[Val], shift: Shift) -> Option<usize> {
	find_pos(values, |val| val.shift == shift)
}

fn new_pos_for(curr_pos: usize, shift: Shift, len: usize) -> usize {
	/* Reduced length:
	 * It turns out that the shuffle is modular wrt to len-1, not to len
	 */
	let redlen: isize = len as isize - 1;
	let mut modshift = shift as isize % redlen;
	if modshift < 0 {
		modshift += redlen;
	}
	return ((curr_pos as isize + modshift) % redlen) as usize;
}

fn shuffle_once(message: &mut Vec<Val>, x: Val) {
	let old_pos = find_id_pos(&message, x.id).expect("Couldn't find id");
	let new_pos = new_pos_for(old_pos, x.shift, message.len());
	message.remove(old_pos);
	message.insert(new_pos, x);
}

fn shuffle_all(input: &[Val], times: u8) -> Vec<Val> {
	let mut message = Vec::from(input);
	for _i in 0..times {
		for x in input {
			shuffle_once(&mut message, *x);
		}
	}
	return message;
}

/* Task 1 */

fn compute_score(decrypted: &[Val]) -> Shift {
	let zero = find_shift_pos(decrypted, 0).expect("Couldn't find 0");
	let len = decrypted.len();

	let mod_at = |i| {
		let val: &Val = &decrypted[(zero + i) % len];
		val.shift
	};

	mod_at(1000) + mod_at(2000) + mod_at(3000)
}

fn task1(input: &[Val]) -> Shift {
	let shuffled = shuffle_all(input, 1);
	compute_score(&shuffled)
}

/* Task 2 */

const DECRYPTION_KEY: i64 = 811589153;

fn task2(input: &[Val]) -> i64 {
	let apply_key = |orig_val: &Val| {
		let naive: i64 = orig_val.shift as i64 * DECRYPTION_KEY;
		let reduced = (naive % (input.len() as i64 - 1)) as Shift;
		Val {
			id:    orig_val.id,
			shift: reduced,
		}
	};

	let mapped_input = input.iter()
						.map(apply_key)
						.collect::<Vec<Val>>();

	let mapped_decrypt = shuffle_all(&mapped_input, 10);

	let retrieve_naive = |mval: &Val| {
		input[mval.id as usize].shift as i64 * DECRYPTION_KEY
	};

	let decrypt_mapped_back = mapped_decrypt.iter()
								.map(retrieve_naive)
								.collect::<Vec<i64>>();

	/* compute_score */
	let zero = decrypt_mapped_back.iter()
					.enumerate()
					.find(|(_idx, x)| **x == 0)
					.map(|(idx, _x)| idx)
					.expect("Couldn't find 0");

	let len = decrypt_mapped_back.len();

	let mod_at = |i| {
		decrypt_mapped_back[(zero + i) % len]
	};

	mod_at(1000) + mod_at(2000) + mod_at(3000)
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
