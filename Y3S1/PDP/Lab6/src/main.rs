use rand::Rng;
use std::sync::Mutex;
use core::sync::atomic::{AtomicBool, Ordering};
use timed_proc_macro::timed;
use rand::thread_rng;
use rand::seq::SliceRandom;
use rayon::prelude::*;

#[derive(Debug)]
struct Graph {
  edges: Vec<Vec<usize>>
}

impl Graph {
  pub fn random(node_count: usize, hamiltonian: bool) -> Graph {
    let mut rng = thread_rng();

    let mut nodes = (0 .. node_count).collect::<Vec<_>>();
    nodes.shuffle(&mut rng);

    let mut edges = vec![vec![]; node_count];

    for i in 0 .. node_count - 1 {
      edges[nodes[i]].push(nodes[i + 1]);
    }

    if hamiltonian {
      edges[nodes[node_count - 1]].push(nodes[0]);

      for _ in 0 .. node_count / 2 {
        let mut i = rng.gen_range(0 .. node_count);
        let mut j = rng.gen_range(0 .. node_count);

        while i == j || edges[i].contains(&j) {
          i = rng.gen_range(0 .. node_count);
          j = rng.gen_range(0 .. node_count);
        }

        edges[i].push(j);
      }
    }

    Graph { edges }
  }

  pub fn find_hamiltonian_cycle_parallel(&self) -> Option<Vec<usize>> {
    let nodes = self.edges.len();

    let done = AtomicBool::new(false);
    let solution = Mutex::new(vec![]);

    (0 .. nodes).into_par_iter().map(|node| {
      let mut path = vec![];
      if self.visit_node(node, &done, &mut path) {
        *solution.lock().unwrap() = path;
      }
    })
    .collect::<Vec<_>>();

    let solution = solution.into_inner().unwrap();

    if done.load(Ordering::SeqCst) && solution.len() == nodes {
      Some(solution)
    } else {
      None
    }
  }

  pub fn visit_node(&self, node: usize, done: &AtomicBool, path: &mut Vec<usize>) -> bool{
    if done.load(Ordering::SeqCst) {
      return false;
    }

    path.push(node);

    if path.len() == self.edges.len() {
      if self.edges[node].contains(&path[0]) {
        done.store(true, Ordering::SeqCst);
        return true;
      } else {
        return false;
      }
    }

    for adj in &self.edges[node] {
      if path.contains(&adj) {
        continue;
      }

      if self.visit_node(*adj, done, path) {
        done.store(true, Ordering::SeqCst);
        return true;
      }

      path.pop();
    }

    return false;
  }
}

#[timed]
fn main() {
  let g = Graph::random(10, true);
  println!("{:?}", g);

  let h = g.find_hamiltonian_cycle_parallel();
  println!("{:?}", h);
}
