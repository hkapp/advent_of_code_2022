use std::collections::BinaryHeap;
use std::cmp;

pub trait AStar: Ord + Sized + Clone {
    type Expansion: Iterator<Item=Self>;

    fn expand(&self) -> Self::Expansion;
    fn prune(&self, curr_best: &Self) -> bool;
}

pub fn astar<T>(start: T) -> T
    where
        T: AStar
{
    let mut curr_best = start.clone();
    let mut pq = BinaryHeap::new();
    pq.push(start);

    let mut pruned: u64 = 0;
    let mut expanded: u64 = 0;

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
