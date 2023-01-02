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

    let mut late_pruned: u64 = 0;
    let mut early_pruned: u64 = 0;
    let mut expanded: u64 = 0;

    let mut next_print = 1000;

    while let Some(curr_node) = pq.pop() {
        if curr_node.prune(&curr_best) {
            late_pruned += 1;
            continue;
        }

        let next_iter = curr_node.expand();
        curr_best = cmp::max(curr_best, curr_node);

        for next in next_iter {
            if next.prune(&curr_best) {
                early_pruned += 1;
            }
            else {
                expanded += 1;
                pq.push(next);
            }
        }

        if (expanded + early_pruned) > next_print {
            println!("astar: expanded {} nodes, pruned {} early and {} late", expanded, early_pruned, late_pruned);
            next_print *= 10;
        }
    }

    println!("astar: expanded {} nodes, pruned {} early and {} late", expanded, early_pruned, late_pruned);

    return curr_best;
}
