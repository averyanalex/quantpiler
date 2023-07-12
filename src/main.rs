#![feature(arc_unwrap_or_clone)]

use std::collections::HashMap;

use itertools::Itertools;
use num::{BigInt, BigRational, One, Zero};

mod bitificator;
mod executor;
mod frontend;
mod graph;
mod unwrapper;

fn main() {
    let ast = frontend::parse();
    println!("AST Done");
    let op = unwrapper::unwrap_func(ast);
    println!("Unwrap done");
    let bits = bitificator::bitificate_op(&op);
    println!("Bitificate done");

    let graph = graph::construct_graph(&bits);
    println!("Unique gates: {}", graph.len());

    let half = BigRational::new(BigInt::one(), BigInt::from(2));
    let half_byte = std::iter::repeat(half).take(8).collect_vec();

    let args = HashMap::from([
        ("a".into(), half_byte.clone()),
        ("b".into(), half_byte.clone()),
        ("c".into(), half_byte.clone()),
        ("d".into(), half_byte),
    ]);

    let result = executor::execute_gates(bits, &args);
    println!(
        "{:?}",
        result
            .iter()
            .rev()
            .skip_while(|b| **b == BigRational::zero())
            .collect_vec()
            .into_iter()
            .rev()
            .collect_vec()
    )
}
