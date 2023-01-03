use std::fs::File;
use std::io::{self, BufRead};
use std::str::FromStr;
use std::fmt;

type Input<T> = io::BufReader<T>;

pub fn run(file_content: Input<File>) {
    let valley = parse(file_content);

    let res1 = task1(&valley);
    println!("Task 1: {}", res1);
    assert_eq!(res1, "20=02=120-=-2110-0=1");

    //let res2 = task2(valley);
    //println!("Task 2: {}", res2);
    //assert_eq!(res2, 798);
}

/* Parsing */

fn parse<T: io::Read>(file_content: Input<T>) -> Vec<Snafu> {
    file_content.lines()
		.map(io::Result::unwrap)
		.map(|s| s.parse().unwrap())
		.collect()
}

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

fn all_digits() -> [Digit; 5] {
	[-2, -1, 0, 1, 2]
}

#[derive(Eq, PartialEq, Debug)]
struct Snafu {
	digits: Vec<Digit>
}

type Decimal = i64;

impl Snafu {
	fn from(n: Decimal) -> Self {
		let (highest_pow, highest_idx) = find_highest_pow(n);
		let mut digits = vec![0; highest_idx+1];

		let mut idx = highest_idx;
		let mut pow = highest_pow;
		let mut rem = n;
		while rem != 0 {
			let curr_digit = find_digit(rem, pow);
			rem -= pow * curr_digit as Decimal;
			digits[idx] = curr_digit;

			if idx == 0 {
				assert_eq!(rem, 0,
					"input: {}, highest_pow: {}, highest_idx: {}, curr_digits: {:?}",
					n, highest_pow, highest_idx, digits);
			}
			else {
				idx -= 1;
			}
			pow /= 5;
		}

		Snafu { digits }
	}

	fn as_decimal(&self) -> Decimal {
		self.digits
			.iter()
			.fold((1, 0 as Decimal),
				|(pow, sum), dig| (pow*5, sum + *dig as Decimal * pow))
			.1
	}
}

fn find_highest_pow(n: Decimal) -> (Decimal, usize) {
	let mut pow = 1;
	let mut idx = 0;
	while pow * 2 < n {
		pow *= 5;
		idx += 1;
	}
	(pow, idx)
}

fn find_digit(rem: Decimal, pow: Decimal) -> Digit {
	IntoIterator::into_iter(all_digits())
		.min_by_key(|dig: &Digit| {
			let diff = rem - pow * (*dig as Decimal);
			diff.abs()
		})
		.unwrap()
}

/* Task 1 */

impl fmt::Display for Snafu {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		fn digit_char(dig: Digit) -> char {
			match dig {
				2  => '2',
				1  => '1',
				0  => '0',
				-1 => '-',
				-2 => '=',
				_  => unreachable!(),
			}
		}

		for dig in self.digits.iter().rev() {
			write!(f, "{}", digit_char(*dig))?;
		}
		Ok(())
	}
}

fn task1(input: &[Snafu]) -> Snafu {
	let dec_res =
		input.iter()
			.map(Snafu::as_decimal)
			.sum();

	Snafu::from(dec_res)
}

/* Unit tests */

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122";

    lazy_static! {
        static ref EXAMPLE_SNAFU: Vec<Snafu> = parse_str(EXAMPLE);
    }

    fn parse_str(s: &str) -> Vec<Snafu> {
		parse(io::BufReader::new(s.as_bytes()))
	}

    #[test]
    fn validate_task1() {
		let expected = "2=-1=0".parse().unwrap();
		assert_eq!(task1(&EXAMPLE_SNAFU), expected);
	}

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

    #[test]
    fn test_from_decimal() {
		fn test_one(s: &str, n: Decimal) {
			assert_eq!(Snafu::from(n), s.parse::<Snafu>().unwrap());
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
