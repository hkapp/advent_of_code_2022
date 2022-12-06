use std::fs::File;
use std::io::{self, BufRead};

const DATA_FILEPATH: &str = "task1.txt";

fn main() {
    let lines = file_lines(DATA_FILEPATH).unwrap();
    let partial_parse = start_parsing(lines);
    let do_first_task = false;
    let strategy =
        if do_first_task {
            println!("Task 1");
            parse_strategy(partial_parse)
        }
        else {
            println!("Task 2");
            parse_strategy2(partial_parse)
        };
    let cost = measure_strategy(strategy);
    println!("Strategy value: {}", cost);
}

fn file_lines(filename: &str) -> io::Result<io::Lines<io::BufReader<File>>> {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

#[derive(Clone, Copy, Debug)]
enum Move {
    Rock,
    Paper,
    Scissors
}

#[derive(Clone, Copy, Debug)]
struct Round {
    opp_move: Move,
    my_move:  Move
}

fn start_parsing(lines: io::Lines<io::BufReader<File>>) -> Box<dyn Iterator<Item=(Move, char)>>
{
    fn parse_one(l: String) -> (Move, char) {
        let mut l_chars = l.chars();
        let opp_char = l_chars.next().unwrap();
        let opp_move =
            match opp_char {
                'A' => Move::Rock,
                'B' => Move::Paper,
                'C' => Move::Scissors,
                _   => panic!("Not a valid char for opp_move: {}", opp_char),
            };

        assert!(l_chars.next() == Some(' '));

        let my_char = l_chars.next().unwrap();

        (opp_move, my_char)
    }

    let iter = lines.map(|l| parse_one(l.unwrap()));
    Box::new(iter)
}

fn measure_round(round: Round) -> u8 {
    fn move_value(m: Move) -> u8 {
        match m {
            Move::Rock     => 1,
            Move::Paper    => 2,
            Move::Scissors => 3,
        }
    }

    fn round_value(round: Round) -> u8 {
        let loss = 0;
        let draw = 3;
        let win  = 6;

        use Move::*;
        match (round.my_move, round.opp_move) {
            (Rock, Rock)     => draw,
            (Rock, Paper)    => loss,
            (Rock, Scissors) => win,

            (Paper, Rock)     => win,
            (Paper, Paper)    => draw,
            (Paper, Scissors) => loss,

            (Scissors, Rock)     => loss,
            (Scissors, Paper)    => win,
            (Scissors, Scissors) => draw,
        }
    }

    println!("{:?} -> {}", round, move_value(round.my_move) + round_value(round));

    move_value(round.my_move) + round_value(round)
}

fn measure_strategy(strategy: Box<dyn Iterator<Item=Round>>) -> u32 {
    strategy
        .map(|round| measure_round(round) as u32)
        .sum()
}

/* Task 1 */

fn parse_strategy(init_parse: Box<dyn Iterator<Item=(Move, char)>>)
    -> Box<dyn Iterator<Item=Round>>
{
    fn make_round(opp_move: Move, my_char: char) -> Round {
        let my_move =
            match my_char {
                'X' => Move::Rock,
                'Y' => Move::Paper,
                'Z' => Move::Scissors,
                _   => panic!("Not a valid char for my_move: {}", my_char),
            };

        Round {
            opp_move,
            my_move
        }
    }

    let iter = init_parse.map(|(m, c)| make_round(m, c));
    Box::new(iter)
}

/* Task 2 */

fn parse_strategy2(init_parse: Box<dyn Iterator<Item=(Move, char)>>)
    -> Box<dyn Iterator<Item=Round>>
{
    fn lose_against(opp_move: Move) -> Move {
        use Move::*;
        match opp_move {
            Rock     => Scissors,
            Paper    => Rock,
            Scissors => Paper,
        }
    }

    fn draw_against(opp_move: Move) -> Move {
        opp_move
    }

    fn win_against(opp_move: Move) -> Move {
        use Move::*;
        match opp_move {
            Rock     => Paper,
            Paper    => Scissors,
            Scissors => Rock,
        }
    }

    fn make_round(opp_move: Move, my_char: char) -> Round {
        let my_move =
            match my_char {
                'X' => lose_against(opp_move),
                'Y' => draw_against(opp_move),
                'Z' => win_against(opp_move),
                _   => panic!("Not a valid char for my_move: {}", my_char),
            };

        Round {
            opp_move,
            my_move
        }
    }

    let iter = init_parse.map(|(m, c)| make_round(m, c));
    Box::new(iter)
}
