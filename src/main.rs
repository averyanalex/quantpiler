use itertools::Itertools;
use rustc_hash::FxHashMap;

use crate::{bitificator::Gate, frontend::Ast};

mod bitificator;
mod circuit;
mod compiler;
mod executor;
mod frontend;
mod graph;
mod unwrapper;

fn main() {
    let ast = frontend::parse();
    println!("AST Done");
    let op = unwrapper::unwrap_func(ast.clone());
    println!("Unwrap done");
    let bits = bitificator::bitificate_op(&op);
    println!("Bitificate done");

    let parents = graph::construct_graph(&bits);
    println!(
        "Unique gates: {}, XOR gates: {}",
        parents.len(),
        parents
            .iter()
            .filter(|(key, _)| matches!(key.as_ref(), Gate::Xor(_)))
            .count()
    );

    for p in parents.iter() {
        let len = p.1.len();
        if len != 1 {
            // println!("Parents: {len}")
        }
    }

    println!("Gates with parents: {}", parents.len());

    // let half: f64 = 0.5;
    let half = 0u8;
    // let half = BigRational::new(BigInt::one(), BigInt::from(2));
    let half_byte = std::iter::repeat(half).take(8).collect_vec();

    let mut args = FxHashMap::default();

    for name in ["a", "b", "c", "d"] {
        args.insert(name.to_string(), half_byte.clone());
    }

    let result = executor::execute_gates(bits.clone(), &args);
    assert_eq!(
        result,
        vec![
            1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1,
            0, 1, 1
        ]
    );
    println!(
        "{:?}",
        result
            .iter()
            .rev()
            .skip_while(|b| **b == 0)
            .collect_vec()
            .into_iter()
            .rev()
            .collect_vec()
    );

    let args_list = if let Ast::Function {
        name: _,
        arguments,
        instructions: _,
    } = ast
    {
        arguments
    } else {
        panic!()
    };

    let circuit = compiler::compile(bits, args_list, parents);
    // println!("{:?}", circuit);
    println!(
        "Compiled, qgates: {}, qubits: {}",
        circuit.gates.len(),
        circuit.next_id
    );

    // println!();
    // println!("{circuit}");
}
