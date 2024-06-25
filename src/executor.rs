use egg::{Id, Language, RecExpr};
use num::BigUint;
use rustc_hash::FxHashMap;

use crate::{
    logic::Logic,
    op::{eval_enode, Op},
};

pub fn execute_logic(logic: &RecExpr<Logic>, args: &FxHashMap<String, Vec<bool>>) -> Vec<bool> {
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
            Logic::Arg(arg) => args[&arg.name][arg.index],
        };
        done.insert(Id::from(idx), result);
    }

    logic.as_ref()[logic.as_ref().len() - 1]
        .children()
        .iter()
        .map(|c| done[c])
        .collect()
}

pub fn execute_op(op: &RecExpr<Op>, args: &FxHashMap<String, BigUint>) -> BigUint {
    let mut done: FxHashMap<Id, BigUint> = FxHashMap::default();

    for (idx, op) in op.as_ref().iter().enumerate() {
        let result = eval_enode(
            op,
            |i| Some(done[&i].clone()),
            |a| Some(args[&a.name].clone()),
        )
        .unwrap();
        done.insert(Id::from(idx), result);
    }

    done[&Id::from(op.as_ref().len() - 1)].clone()
}
