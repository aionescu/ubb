use std::sync::Barrier;
use std::sync::Arc;
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
  pub fn random(node_count: usize) -> Graph {
    let mut nodes = (0 .. node_count).collect::<Vec<_>>();
    nodes.shuffle(&mut thread_rng());

    let mut edges = vec![vec![]; node_count];

    for i in 0 .. node_count - 1 {
      edges[nodes[i]].push(nodes[i + 1]);
    }

    edges[nodes[node_count - 1]].push(nodes[0]);

    Graph { edges }
  }

  pub fn find_hamiltonian_cycle(&self) -> Option<Vec<usize>> {
    let mut visited = vec![false; self.edges.len()];
    let mut stack = vec![];
    let mut current = 0;

    stack.push(current);
    visited[current] = true;

    while let Some(next) = self.edges[current].iter().find(|&&x| !visited[x]) {
      stack.push(*next);
      visited[*next] = true;
      current = *next;
    }

    if visited[current] {
      Some(stack)
    } else {
      None
    }
  }

  pub fn find_hamiltonian_cycle_parallel(&self) -> Vec<usize> {
    let node_count = self.edges.len();

    let done = AtomicBool::new(false);
    let path = Mutex::new(vec![]);

    (0 .. node_count).into_par_iter().for_each(|node| {
      self.visit_node(node, &done, &path);
    });

    while !done.load(Ordering::SeqCst) {
    }

    path.into_inner().unwrap()
  }

  pub fn visit_node(&self, node: usize, done: &AtomicBool, path: &Mutex<Vec<usize>>) {
    if done.load(Ordering::SeqCst) {
      return;
    }

    let path_len = {
      let mut path = path.lock().unwrap();
      path.push(node);
      path.len()
    };

    if path_len != self.edges.len() {
      let path_ = path.lock().unwrap().clone();

      self.edges[node].iter().find(|x| !path_.contains(x)).map(|&x| {
        self.visit_node(x, &done, &path);
      });
    }

    done.store(true, Ordering::SeqCst);
  }
}

#[timed]
fn main() {
  let g = Graph::random(10);
  println!("{:?}", g);

  let h = g.find_hamiltonian_cycle().unwrap();
  println!("{:?}", h);

  let h = g.find_hamiltonian_cycle_parallel();
  println!("{:?}", h);
}
