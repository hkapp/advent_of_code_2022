use std::fs::File;
use std::io::{self, BufRead};
use std::collections::HashMap;
use std::str::FromStr;

type Input<T> = io::BufReader<T>;

pub fn run(file_content: Input<File>) {
    //let (map_proj, moves) = parse(file_content);

    //let res1 = task1(&map_proj, &moves);
    //println!("Task 1: {}", res1);
    //assert_eq!(res1, 88226);

    //let res2 = task2(&map_proj, &moves);
    //println!("Task 2: {}", res2);
    //assert_eq!(res2, 57305);
}

/* Parsing */

//fn parse<T: io::Read>(file_content: Input<T>) -> (MapProjection, Vec<Move>) {
    //let mut line_iter = file_content.lines();

    //// Parse the planet
    //let mut curr_row = 1;
    //let mut map_proj = MapProjection::new();

    //while let Some(line_res) = line_iter.next() {
		//let line = line_res.unwrap();
		//if line.is_empty() {
			//// This is the separator
			//// What follows is the move sequence
			//break;
		//}

		//parse_map_proj_row(&mut map_proj, line, curr_row);
		//curr_row += 1;
	//}

	//// Parse the move sequence
	//let move_line = line_iter.next().unwrap().unwrap();
	//assert!(line_iter.next().is_none());
	//let mut moves = Vec::new();
	//for segment in segregate_str(&move_line, char::is_ascii_digit) {
		//let m =
			//match segment {
				//Ok(digits) => Move::Walk(digits.parse().unwrap()),
				//Err(chars) => Move::Turn(chars.parse().unwrap()),
			//};
		//moves.push(m);
	//}

	//return (map_proj, moves);
//}

/* Coordinate system */

/* Unit tests */

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5";

    lazy_static! {
        static ref EXAMPLE_PLANET: MapProjection =
			parse(io::BufReader::new(EXAMPLE.as_bytes())).0;
        static ref EXAMPLE_MOVES: Vec<Move> =
			parse(io::BufReader::new(EXAMPLE.as_bytes())).1;

		static ref EXAMPLE_CUBE: Cube = build_test_cube();
    }
	const A: Pos = Pos { row: 6, column: 12 };
	const B: Pos = Pos { row: 9, column: 15 };
	const C: Pos = Pos { row:12, column: 11 };
	const D: Pos = Pos { row: 8, column:  2 };

	fn build_test_cube() -> Cube {
		/*      +---+
			    | 1 |
		+---+---+---+
		| 2 | 3 | 4 |
		+---+---+---+---+
		        | 5 | 6 |
		        +---+---+
		*/
		let layout =
			[
				vec![None,    None,    Some(1), None],
				vec![Some(2), Some(3), Some(4), None],
				vec![None,    None,    Some(5), Some(6)],
			];

		Cube {
			faces: build_faces_from_layout(4, &layout),
			edges: build_test_edges()
		}
	}

	/*
			 +-------------+
			 :             :
			 : +---------+ :
			 : :         : :
			 : :       +-^-V-+
			 : :   +--->     >-----------+
			 : :   :   |  1  |           :
			 : :   : +-<     <--------+  :
		   +-^-V-+-^-V-+-^-V-+        :  :
	 +----->     >     >     >---+    :  :
	 :     |  2  |  3  |  4  |   :    :  :
	 :  +--<     <     <     <-+ :    :  :
	 :  :  +-^-V-+-^-V-+-^-V-+-^-V-+  :  :
	 :  :    : :   : +->     >     >--+  :
	 :  :    : :   :   |  5  |  6  |     :
	 :  :    : :   +---<     <     <-----+
	 :  :    : :       +-^-V-+-^-V-+
	 :  :    : :         : :   : :
	 :  :    : +---------+ :   : :
	 :  :    :             :   : :
	 :  :    +-------------+   : :
	 :  :                      : :
	 :  +----------------------+ :
	 :                           :
	 +---------------------------+
	*/

	/*
	 When    |      Up        |      Down      |      Left      |      Right     |
	 exiting | Go to | Rotate | Go to | Rotate | Go to | Rotate | Go to | Rotate |
	---------+----------------+----------------+----------------+----------------+
	   1     |   2       x2   |   4       No   |   3       KW   |   6       x2   |
	   2     |   1       x2   |   5       x2   |   6       CW   |   3       No   |
	   3     |   1       CW   |   5       KW   |   2       No   |   4       No   |
	   4     |   1       No   |   5       No   |   3       No   |   6       CW   |
	   5     |   4       No   |   2       x2   |   3       CW   |   6       No   |
	   6     |   4       KW   |   2       KW   |   5       No   |   1       x2   |

	Legend:
	  No: don't rotate
	  CW: clockwise
	  KW: counter-clockwise
	  2x: full turn

	  Note that rotating requires changing the facing direction
	  but also the coordinates when entering the new face.
	*/
	fn build_test_edges() -> CubeEdges {
		use Rotation::*;
		HashMap::from(
			[
				// 1     |   2       x2   |   4       No   |   3       KW   |   6       x2   |
				((1, Up),    Edge { destination: 2, rotation: vec![Clockwise, Clockwise] }),
				((1, Down),  Edge { destination: 4, rotation: vec![] }),
				((1, Left),  Edge { destination: 3, rotation: vec![CounterClockwise] }),
				((1, Right), Edge { destination: 6, rotation: vec![Clockwise, Clockwise] }),
				// 2     |   1       x2   |   5       x2   |   6       CW   |   3       No   |
				((2, Up),    Edge { destination: 1, rotation: vec![Clockwise, Clockwise] }),
				((2, Down),  Edge { destination: 5, rotation: vec![Clockwise, Clockwise] }),
				((2, Left),  Edge { destination: 6, rotation: vec![Clockwise] }),
				((2, Right), Edge { destination: 3, rotation: vec![] }),
				// 3     |   1       CW   |   5       KW   |   2       No   |   4       No   |
				((3, Up),    Edge { destination: 1, rotation: vec![Clockwise] }),
				((3, Down),  Edge { destination: 5, rotation: vec![CounterClockwise] }),
				((3, Left),  Edge { destination: 2, rotation: vec![] }),
				((3, Right), Edge { destination: 4, rotation: vec![] }),
				// 4     |   1       No   |   5       No   |   3       No   |   6       CW   |
				((4, Up),    Edge { destination: 1, rotation: vec![] }),
				((4, Down),  Edge { destination: 5, rotation: vec![] }),
				((4, Left),  Edge { destination: 3, rotation: vec![] }),
				((4, Right), Edge { destination: 6, rotation: vec![Clockwise] }),
				// 5     |   4       No   |   2       x2   |   3       CW   |   6       No   |
				((5, Up),    Edge { destination: 4, rotation: vec![] }),
				((5, Down),  Edge { destination: 2, rotation: vec![Clockwise, Clockwise] }),
				((5, Left),  Edge { destination: 3, rotation: vec![Clockwise] }),
				((5, Right), Edge { destination: 6, rotation: vec![] }),
				// 6     |   4       KW   |   2       KW   |   5       No   |   1       x2   |
				((6, Up),    Edge { destination: 4, rotation: vec![CounterClockwise] }),
				((6, Down),  Edge { destination: 2, rotation: vec![CounterClockwise] }),
				((6, Left),  Edge { destination: 5, rotation: vec![] }),
				((6, Right), Edge { destination: 1, rotation: vec![Clockwise, Clockwise] }),
			]
		)
	}

    #[test]
    fn validate_task1() {
        assert_eq!(task1(&EXAMPLE_PLANET, &EXAMPLE_MOVES), 6032);
    }

    #[test]
    fn validate_task2() {
        assert_eq!(task2_param(&EXAMPLE_PLANET, build_test_cube(), &EXAMPLE_MOVES), 5031);
    }

    #[test]
    fn test_cube_physics() {
		/* Example:
        ...#
        .#..
        #...
        ....
...#.......#
........#..A>-+
..#....#....  :
.D........#.  V
 ^      ...#..B.
 :      .....#..
 :      .#......
 :      ..C...#.
 :        V
 +--------+
        */

        assert_eq!(
			EXAMPLE_CUBE.wrap_around(A, Right),
			(B, Down));
        assert_eq!(
			EXAMPLE_CUBE.wrap_around(C, Down),
			(D, Up));
    }

    #[test]
    fn test_rotate_within_face() {
		/* Clockwise:
           .2.#
           ...3
           1#..
           ..C.

           Counter-clockwise is the opposite
        */
        let p1 = Pos {
			column: C.column - 2,
			row:    C.row - 1,
		};
        let p2 = Pos {
			column: C.column - 1,
			row:    p1.row - 2,
		};
        let p3 = Pos {
			column: C.column + 1,
			row:    p2.row + 1,
		};

		let face = EXAMPLE_CUBE.get_face(identify_face(&EXAMPLE_CUBE, C).unwrap());
		use Rotation::*;

        assert_eq!(
			rotate_within_face(face, C, Clockwise),
			p1);
        assert_eq!(
			rotate_within_face(face, p1, Clockwise),
			p2);
        assert_eq!(
			rotate_within_face(face, p2, Clockwise),
			p3);
        assert_eq!(
			rotate_within_face(face, p3, Clockwise),
			C);

        assert_eq!(
			rotate_within_face(face, C, CounterClockwise),
			p3);
        assert_eq!(
			rotate_within_face(face, p3, CounterClockwise),
			p2);
        assert_eq!(
			rotate_within_face(face, p2, CounterClockwise),
			p1);
        assert_eq!(
			rotate_within_face(face, p1, CounterClockwise),
			C);
    }

    fn test_full_circle(cube: Cube, start_pos: Pos) {
		let mut map_proj = HashMap::new();
		for face in cube.faces.iter() {
			for row in face.top_row..(face.bot_row+1) {
				for column in face.left_col..(face.right_col+1) {
					map_proj.insert(Pos {row, column}, Tile::Open);
				}
			}
		}
		let recurr_time = 50*4;

		let mut astronaut = Astronaut {
			planet: Planet {
				shape_3d:   cube,
				terrain_2d: &map_proj,
			},
			curr_dir: Down,
			curr_pos: start_pos
		};
		astronaut.walk(recurr_time);
		assert_eq!(astronaut.curr_pos, start_pos);

		astronaut.curr_dir = Left;
		astronaut.walk(recurr_time);
		assert_eq!(astronaut.curr_pos, start_pos);

		astronaut.curr_dir = Right;
		astronaut.walk(recurr_time);
		assert_eq!(astronaut.curr_pos, start_pos);

		astronaut.curr_dir = Up;
		astronaut.walk(recurr_time);
		assert_eq!(astronaut.curr_pos, start_pos);
	}

    #[test]
    fn test_real_cube() {
		let cube = Cube::new();
		let start_pos = Pos {
			row:    cube.faces[0].top_row + 1,
			column: cube.faces[0].left_col + 1,
		};
		test_full_circle(cube, start_pos);
    }

}
