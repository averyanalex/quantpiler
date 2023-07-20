use std::{ops::Sub, rc::Rc};

use num::{One, Zero};
use rustc_hash::FxHashMap;

use crate::bitificator::Gate;

fn execute_gates_rec<T>(
    gate: &Rc<Gate>,
    args: &FxHashMap<String, Vec<T>>,
    cache: &mut FxHashMap<Rc<Gate>, T>,
) -> T
where
    T: Zero + One + Sub<Output = T>,
    T: Clone,
{
    match cache.get(gate) {
        Some(result) => result.clone(),
        None => {
            let result = match gate.as_ref() {
                Gate::Argument { name, index } => args[name][*index as usize].clone(),
                Gate::Constant(value) => {
                    if *value {
                        T::one()
                    } else {
                        T::zero()
                    }
                }
                Gate::Not(gate) => T::one() - execute_gates_rec(gate, args, cache),
                Gate::And(gates) => {
                    assert!(!gates.is_empty());
                    gates
                        .iter()
                        .map(|g| execute_gates_rec(g, args, cache))
                        .fold(T::one(), |acc, x| acc * x)
                }
                Gate::Xor(gates) => {
                    assert!(!gates.is_empty());
                    gates
                        .iter()
                        .map(|g| execute_gates_rec(g, args, cache))
                        .fold(T::zero(), |acc, x| {
                            (acc.clone() * (T::one() - x.clone())) + ((T::one() - acc) * x)
                        })
                }
            };
            cache.insert(gate.clone(), result.clone());
            result
        }
    }
}

pub fn execute_gates<T>(gates: Vec<Rc<Gate>>, args: &FxHashMap<String, Vec<T>>) -> Vec<T>
where
    T: Zero + One + Sub<Output = T>,
    T: Clone,
{
    let mut cache = FxHashMap::default();
    gates
        .iter()
        .map(|g| execute_gates_rec(g, args, &mut cache))
        .collect()
}
