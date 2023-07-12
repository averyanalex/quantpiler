use itertools::Itertools;
// use num::{BigInt, BigRational, One, Zero};
use rustc_hash::FxHashMap;

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

    let half: f64 = 0.5;
    // let half = BigRational::new(BigInt::one(), BigInt::from(2));
    let half_byte = std::iter::repeat(half).take(8).collect_vec();

    let mut args = FxHashMap::default();

    for name in ["a", "b", "c", "d"] {
        args.insert(name.to_string(), half_byte.clone());
    }

    let result = executor::execute_gates(bits, &args);
    println!(
        "{:?}",
        result
            .iter()
            .rev()
            .skip_while(|b| **b == 0.0)
            .collect_vec()
            .into_iter()
            .rev()
            .collect_vec()
    )
}
