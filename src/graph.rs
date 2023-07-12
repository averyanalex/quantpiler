use std::{collections::HashSet, rc::Rc};

use crate::bitificator::Gate;

fn add_to_hashset(gate: Rc<Gate>, set: &mut HashSet<Rc<Gate>>) {
    if !set.contains(&gate) {
        match gate.as_ref() {
            Gate::Argument { name: _, index: _ } => {}
            Gate::Constant(_) => {}
            Gate::Not(arg) => add_to_hashset(arg.clone(), set),
            Gate::And(args) => args.iter().for_each(|arg| add_to_hashset(arg.clone(), set)),
            Gate::Or(args) => args.iter().for_each(|arg| add_to_hashset(arg.clone(), set)),
            Gate::Xor(args) => args.iter().for_each(|arg| add_to_hashset(arg.clone(), set)),
        }
        set.insert(gate);
    }
}

pub fn construct_graph(gates: &[Rc<Gate>]) -> HashSet<Rc<Gate>> {
    let mut set = HashSet::new();
    for gate in gates {
        add_to_hashset(gate.clone(), &mut set);
    }
    set
}
