use std::fs::File;
use std::io::{self, BufRead};
use std::iter::Iterator;
use std::collections::BinaryHeap;
use std::cmp;
use std::ops::{Index, IndexMut};
use std::convert::{TryFrom, TryInto};
use std::cell::Cell;

type Input<T> = io::BufReader<T>;

pub fn run(file_content: Input<File>) {
    let parsed = parse(file_content);

    let res1 = task1(&parsed);
    println!("Task 1: {}", res1);
    //assert_eq!(res1, 3650);

    //let res2 = task2(&parsed);
    //println!("Task 2: {}", res2);
    //assert_eq!(res2, 2118);
}

/* Parsing */

fn parse<T: io::Read>(file_content: Input<T>) -> Vec<Blueprint> {
    #[allow(non_snake_case)]
    fn parse_blueprint(line: String) -> Blueprint {
        use regex::Regex;
        let re = Regex::new(
r"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian."
        ).unwrap();

        let numbers = re.captures(&line).expect("Couldn't match the regex");
        assert_eq!(numbers.len(), 7+1);

        // Warning! index 0 stores the whole match
        let number_at = |n| {
            numbers.get(n+1 as usize).unwrap().as_str().parse().unwrap()
        };

        let mut bp = new_blueprint();
        /* Number 0 is blueprint id */
        /* Number 1 is Ore cost of Ore robot */
        bp[Ore][Ore] = number_at(1);
        /* Number 2 is Ore cost of Clay robot */
        bp[Clay][Ore] = number_at(2);
        /* Number 3 is Ore cost of Obsidian robot */
        bp[Obsidian][Ore] = number_at(3);
        /* Number 4 is Clay cost of Obsidian robot */
        bp[Obsidian][Clay] = number_at(4);
        /* Number 5 is Ore cost of Geode robot */
        bp[Geode][Ore] = number_at(5);
        /* Number 6 is Obsidian cost of Geode robot */
        bp[Geode][Obsidian] = number_at(6);

        return bp;
    }

    file_content
        .lines()
        .map(|r| r.unwrap())
        .map(parse_blueprint)
        .collect::<Vec<Blueprint>>()
}

/* Mineral */

#[derive(Clone, Copy)]
enum Mineral {
    Ore,
    Clay,
    Obsidian,
    Geode
}
use Mineral::*;

const ALL_MINERALS: [Mineral; NUM_MINERALS] =
    [Ore, Clay, Obsidian, Geode];

impl Mineral {
    fn all() -> &'static [Mineral] {
        &ALL_MINERALS
    }
}

impl Into<usize> for Mineral {
    fn into(self) -> usize {
        match self {
            Ore      => 0,
            Clay     => 1,
            Obsidian => 2,
            Geode    => 3,
        }
    }
}

impl TryFrom<usize> for Mineral {
    type Error = ();

    fn try_from(x: usize) -> Result<Self, Self::Error> {
        if x < NUM_MINERALS {
            Ok(ALL_MINERALS[x])
        }
        else {
            Err(())
        }
    }
}

const NUM_MINERALS: usize = 4;
#[derive(Clone, Debug)]
struct MArray<T>([T; NUM_MINERALS]);

impl<T: Clone> MArray<T> {
    fn new_all(val: &T) -> Self {
        MArray([val.clone(), val.clone(), val.clone(), val.clone()])
    }
}

impl<T> Index<Mineral> for MArray<T> {
    type Output = T;

    fn index(&self, midx: Mineral) -> &Self::Output {
        &self.0[midx as usize]
    }
}

impl<T> IndexMut<Mineral> for MArray<T> {
    fn index_mut(&mut self, midx: Mineral) -> &mut Self::Output {
        &mut self.0[midx as usize]
    }
}

/* Resource */

type Stock = u16;
type Prod  = u8;

#[derive(Clone, Copy, Debug)]
struct Resource {
    stock:      Stock,
    production: Prod,
}

impl Resource {
    fn new() -> Self {
        Resource {
            stock:      0,
            production: 0,
        }
    }

    fn inc_production(&mut self) {
        self.production += 1;
    }

    fn produce(&mut self) {
        self.stock += self.production as Stock;
    }
}

/* Warehouse */

#[derive(Clone)]
struct Warehouse {
    resources: MArray<Resource>
}

impl Warehouse {
    fn start_state() -> Self {
        let mut init =
            Warehouse {
                resources: MArray([Resource::new(); NUM_MINERALS]),
            };
        init.resources[Mineral::Ore].production = 1;
        return init;
    }
}

/* A star */

trait AStar: Ord + Sized + Clone {
    type Expansion: Iterator<Item=Self>;

    fn expand(&self) -> Self::Expansion;
    fn prune(&self, curr_best: &Self) -> bool;
}

fn astar<T>(start: T) -> T
    where
        T: AStar
{
    let mut curr_best = start.clone();
    let mut pq = BinaryHeap::new();
    pq.push(start);

    let mut pruned = 0;
    let mut expanded = 0;

    while let Some(curr_node) = pq.pop() {
        if curr_node.prune(&curr_best) {
            pruned += 1;
            continue;
        }
        for next in curr_node.expand() {
            expanded += 1;
            pq.push(next);
        }
        curr_best = cmp::max(curr_best, curr_node);
    }

    println!("astar: expanded {} nodes, pruned {}", expanded, pruned);

    return curr_best;
}

/* Blueprint */

/* For each mineral, tell the cost of producing a robot of that type */
type Cost = MArray<Stock>;
type Blueprint = MArray<Cost>;

fn new_blueprint() -> Blueprint {
    MArray::new_all(&MArray::new_all(&0))
}

/* Game */

type Robot = Mineral;
type Score = Stock;
type Time  = u8;

struct Game<'a> {
    costs:     &'a Blueprint,
    state:     Warehouse,
    time_left: Time,
    pot_cache: Cell<Option<Score>>
}

impl<'a> Game<'a> {
    fn new(costs: &'a Blueprint, time_left: Time) -> Self {
        Game {
            costs,
            state:     Warehouse::start_state(),
            time_left,
            pot_cache: Cell::new(None)
        }
    }

    fn can_buy_robot(&self, robot: Robot) -> bool {
        let cost = &self.costs[robot];
        let resources = &self.state.resources;
        for m in Mineral::all() {
            if cost[*m] > resources[*m].stock {
                return false;
            }
        }
        return true;
    }

    fn pay_for_robot(&mut self, robot: Robot) {
        // Note: this function is unchecked (assumes we can buy the robot)
        let cost = &self.costs[robot];
        let resources = &mut self.state.resources;
        for m in Mineral::all() {
            resources[*m].stock -= cost[*m];
        }
    }

    fn one_round(&mut self, action: Option<Robot>) {
        // Pay for the robot (if any)
        if let Some(robot) = action {
            self.pay_for_robot(robot);
        }
        // Make existing robots work
        for m in Mineral::all() {
            self.state.resources[*m].produce();
        }
        // Add the robot (if any)
        if let Some(robot) = action {
            self.state.resources[robot].inc_production();
        }

        // Reduce time left
        self.time_left -= 1;
    }

    fn score(&self) -> Score {
        self.state.resources[Geode].stock
    }

    fn potential(&self) -> Score {
        if let Some(p) = self.pot_cache.get() {
            p
        }
        else {
            let p = compute_potential(self);
            self.pot_cache.set(Some(p));
            p
        }
    }

    fn is_over(&self) -> bool {
        self.time_left == 0
    }
}

impl<'a> Clone for Game<'a> {
    fn clone(&self) -> Self {
        Game {
            costs:     self.costs,
            state:     self.state.clone(),
            time_left: self.time_left,
            pot_cache: Cell::new(None)
        }
    }
}

/* AStar for Game */

/* Part 1: Ord */

impl<'a> PartialEq for Game<'a> {
    fn eq(&self, other: &Self) -> bool {
        Ord::cmp(self, other) == cmp::Ordering::Equal
    }
}

impl<'a> Eq for Game<'a> { }

macro_rules! seq_ord {
    ($l:expr, $r:expr; $f:expr) => {
        Ord::cmp(&$f($l), &$f($r))
    };

    ($l:expr, $r:expr; $f:expr, $($fs:expr),+) => {
        match Ord::cmp(&$f($l), &$f($r)) {
            cmp::Ordering::Equal => seq_ord!($l, $r; $($fs),+),
            c@_ => c
        }
    };
}

impl<'a> PartialOrd for Game<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        /* We can only compare Games that use the same Blueprint */
        let bp1 = self.costs as *const Blueprint;
        let bp2 = other.costs as *const Blueprint;
        if bp1 == bp2 {
            Some(
                seq_ord!{
                    self, other;
                    |g: &Self| g.state.resources[Geode].stock,
                    |g: &Self| g.potential(),
                    |g: &Self| g.state.resources[Geode].production,
                    |g: &Self| g.state.resources[Obsidian].stock,
                    |g: &Self| g.state.resources[Obsidian].production,
                    |g: &Self| g.state.resources[Clay].stock,
                    |g: &Self| g.state.resources[Clay].production,
                    |g: &Self| g.state.resources[Ore].stock,
                    |g: &Self| g.state.resources[Ore].production
                })
        }
        else {
            None
        }
    }
}

impl<'a> Ord for Game<'a> {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        /* Note: this will fail if we're not comparing games
         * with the same blueprint.
         */
        self.partial_cmp(other).unwrap()
    }
}

/* Part 2: state expansion */

struct Explorer<'a> {
    base_game: Game<'a>,
    curr_idx:  usize
}

impl<'a> Explorer<'a> {
    fn new(base_game: Game<'a>) -> Self {
        Explorer {
            base_game,
            curr_idx: 0
        }
    }

    fn find_next_move(&mut self) -> Option<Option<Mineral>> {
        // TODO we can reduce the search space here
        // 1. No need to build a robot if production exceeds cost of robots that need this resource
        // 2. If you can build a robot, always do so
        if self.curr_idx > NUM_MINERALS || self.base_game.is_over() {
            /* No more moves */
            return None;
        }
        else if self.curr_idx == NUM_MINERALS {
            /* Don't buy anything, just let time pass */
            self.curr_idx += 1;
            return Some(None);
        }
        else {
            let m = self.curr_idx.try_into().unwrap();
            self.curr_idx += 1;
            if self.base_game.can_buy_robot(m) {
                return Some(Some(m));
            }
            else {
                /* continue searching */
                return self.find_next_move();
            }
        }
    }
}

impl<'a> Iterator for Explorer<'a> {
    type Item = Game<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.find_next_move() {
            Some(next_move) => {
                let mut next_game_state = self.base_game.clone();
                next_game_state.one_round(next_move);
                Some(next_game_state)
            }
            None => None
        }
    }

}

/* Part 3: potential */

/* We compute the theoretical max of a game state by running under the following assumptions:
 * If the agent can pay for one robot of a given kind, it can produce one robot of that kind each round
 * Only Clay costs Ore
 *
 * Under these assumptions, the theoretical optimum is given by building robots down the graph
 * of cost requirements.
 *
 * Requirements:
 * Ore            -> Ore
 * Ore            -> Clay
 * Ore + Clay     -> Obsidian
 * Ore + Obsidian -> Geode
 */
fn compute_potential(game: &Game) -> Score {
    let mut rollout_game = game.clone();
    /* Ignore the Ore cost of Obsidian and Geode */
    let mut dep_costs = rollout_game.costs.clone();
    dep_costs[Obsidian][Ore] = 0;
    dep_costs[Geode][Ore] = 0;

    /* When buying robots, pretend that they don't cost anything */
    let free = MArray([0; NUM_MINERALS]);
    let zero_costs = MArray([free.clone(), free.clone(), free.clone(), free.clone()]);

    let produce = |m, g: &Game| {
        g.state.resources[m].production > 0
    };

    let mut until_can_produce = |target, dependency| {
        while !produce(target, &rollout_game) && !rollout_game.is_over() {
            /* Add 1 `dependency` robot every turn until we have enough for one `target` robot */
            rollout_game.costs = &dep_costs;
            let next_robot = if rollout_game.can_buy_robot(target) { target } else { dependency };
            rollout_game.costs = &zero_costs;
            rollout_game.one_round(Some(next_robot));
        }
    };

    until_can_produce(Clay, Ore);
    until_can_produce(Obsidian, Clay);
    until_can_produce(Geode, Obsidian);

    let mut geodes_until_the_end = || {
        rollout_game.costs = &zero_costs;
        while !rollout_game.is_over() {
            /* Add 1 Geode robot every turn */
            rollout_game.one_round(Some(Geode));
        }
    };

    geodes_until_the_end();

    return rollout_game.score();
}

impl<'a> AStar for Game<'a> {
    type Expansion = Explorer<'a>;

    fn expand(&self) -> Self::Expansion {
       Explorer::new(self.clone())
    }

    fn prune(&self, curr_best: &Self) -> bool {
        // We prune the current state if it's score + potential is worse
        // than the current best score
        (self.score() + self.potential()) <= curr_best.score()
    }
}

fn optimal_for_blueprint(bp: &Blueprint) -> Score {
    astar(Game::new(bp, 24)).score()
}

/* Unit tests */

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "\
    Blueprint 1: \
        Each ore robot costs 4 ore. \
        Each clay robot costs 2 ore. \
        Each obsidian robot costs 3 ore and 14 clay. \
        Each geode robot costs 2 ore and 7 obsidian.
    Blueprint 2: \
        Each ore robot costs 2 ore. \
        Each clay robot costs 3 ore. \
        Each obsidian robot costs 3 ore and 8 clay. \
        Each geode robot costs 3 ore and 12 obsidian.";

    lazy_static! {
        static ref EXAMPLE_BLUEPRINTS: Vec<Blueprint> =
            parse(io::BufReader::new(EXAMPLE.as_bytes()));
    }

    #[test]
    fn validate_bp1() {
        let bps: &Vec<Blueprint> = &EXAMPLE_BLUEPRINTS;
        println!("{:?}", bps);
        assert_eq!(optimal_for_blueprint(&EXAMPLE_BLUEPRINTS[0]), 9);
    }

    #[test]
    fn validate_bp2() {
        assert_eq!(optimal_for_blueprint(&EXAMPLE_BLUEPRINTS[1]), 12);
    }
}
