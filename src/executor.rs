// use std::{ops::Sub, rc::Rc};

use egg::{Id, Language, RecExpr};
// use num::{One, Zero};
use rustc_hash::FxHashMap;

use crate::logic::Logic;

pub fn execute_gates(logic: &RecExpr<Logic>, args: &FxHashMap<String, Vec<bool>>) -> Vec<bool>
// where
//     T: Zero + One + Sub<Output = T>,
//     T: Clone,
{
    let mut done: FxHashMap<Id, bool> = FxHashMap::default();

    for (idx, op) in logic.as_ref().iter().enumerate() {
        let result = match op {
            Logic::Xor(args) => {
                assert!(args.len() >= 2);
                args.iter().fold(false, |acc, i| acc ^ done[i])
            }
            Logic::And(args) => {
                assert!(args.len() >= 2);
                args.iter().fold(true, |acc, i| acc & done[i])
            }
            Logic::Not(arg) => !done[arg],
            Logic::Register(_) => continue,
            Logic::Const(val) => *val,
            Logic::Arg(arg) => args[&arg.name][arg.index as usize],
        };
        done.insert(Id::from(idx), result);
    }

    logic.as_ref()[logic.as_ref().len() - 1]
        .children()
        .iter()
        .map(|c| done[c])
        .collect()
}
