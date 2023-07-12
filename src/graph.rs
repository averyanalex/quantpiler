use std::rc::Rc;

use rustc_hash::FxHashSet;

use crate::bitificator::Gate;

fn add_to_set(gate: Rc<Gate>, set: &mut FxHashSet<Rc<Gate>>) {
    if !set.contains(&gate) {
        match gate.as_ref() {
            Gate::Argument { name: _, index: _ } => {}
            Gate::Constant(_) => {}
            Gate::Not(arg) => add_to_set(arg.clone(), set),
            Gate::And(args) => args.iter().for_each(|arg| add_to_set(arg.clone(), set)),
            Gate::Or(args) => args.iter().for_each(|arg| add_to_set(arg.clone(), set)),
            Gate::Xor(args) => args.iter().for_each(|arg| add_to_set(arg.clone(), set)),
        }
        set.insert(gate);
    }
}

pub fn construct_graph(gates: &[Rc<Gate>]) -> FxHashSet<Rc<Gate>> {
    let mut set = FxHashSet::default();
    for gate in gates {
        add_to_set(gate.clone(), &mut set);
    }
    set
}
