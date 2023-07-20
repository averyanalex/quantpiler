use std::rc::Rc;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::bitificator::Gate;

fn add_to_set(
    gate: Rc<Gate>,
    parent: Option<Rc<Gate>>,
    parents: &mut FxHashMap<Rc<Gate>, FxHashSet<Rc<Gate>>>,
) {
    if !parents.contains_key(&gate) {
        parents.insert(gate.clone(), FxHashSet::default());

        match gate.as_ref() {
            Gate::Argument { name: _, index: _ } => {}
            Gate::Constant(_) => {}
            Gate::Not(arg) => add_to_set(arg.clone(), Some(gate.clone()), parents),
            Gate::And(args) => args
                .iter()
                .for_each(|arg| add_to_set(arg.clone(), Some(gate.clone()), parents)),
            Gate::Xor(args) => args
                .iter()
                .for_each(|arg| add_to_set(arg.clone(), Some(gate.clone()), parents)),
        }

        if let Some(parent) = parent {
            parents
                .entry(gate)
                .and_modify(|p| {
                    p.insert(parent.clone());
                    // println!("Insert {} parent", p.len());
                })
                .or_insert_with(|| panic!());
        }
    }
}

pub fn construct_graph(gates: &[Rc<Gate>]) -> FxHashMap<Rc<Gate>, FxHashSet<Rc<Gate>>> {
    let mut parents = FxHashMap::default();
    for gate in gates {
        add_to_set(gate.clone(), None, &mut parents);
    }
    parents
}
