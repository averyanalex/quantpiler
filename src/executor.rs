use egg::{Id, Language, RecExpr};
use itertools::Itertools;
use num::{BigUint, FromPrimitive, One, Zero};
use rustc_hash::FxHashMap;

use crate::{logic::Logic, op::Op};

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

pub fn execute_op(op: &RecExpr<Op>, args: &FxHashMap<String, BigUint>) -> BigUint {
    let mut done: FxHashMap<Id, BigUint> = FxHashMap::default();

    for (idx, op) in op.as_ref().iter().enumerate() {
        let result = match op {
            Op::Not(arg) => {
                let digits = done[arg].iter_u64_digits().map(|d| !d).collect_vec();
                assert_eq!(digits.len(), 1);
                BigUint::from_u64(!digits[0]).unwrap()
            }
            Op::Xor([a, b]) => done[a].clone() ^ done[b].clone(),
            Op::Or([a, b]) => done[a].clone() | done[b].clone(),
            Op::And([a, b]) => done[a].clone() & done[b].clone(),
            Op::Shr([a, b]) => done[a].clone() >> u128::try_from(done[b].clone()).unwrap(),
            Op::Shl([a, b]) => done[a].clone() << u128::try_from(done[b].clone()).unwrap(),
            Op::Add([a, b]) => done[a].clone() + done[b].clone(),
            Op::Sub([a, b]) => done[a].clone() - done[b].clone(),
            Op::Mul([a, b]) => done[a].clone() * done[b].clone(),
            Op::Div([a, b]) => done[a].clone() / done[b].clone(),
            Op::Rem([a, b]) => done[a].clone() % done[b].clone(),
            Op::Eq([a, b]) => {
                if done[a] == done[b] {
                    BigUint::one()
                } else {
                    BigUint::zero()
                }
            }
            Op::Ternary([cond, then, or]) => {
                if done[cond].is_one() {
                    done[then].clone()
                } else if done[cond].is_zero() {
                    done[or].clone()
                } else {
                    panic!("expected condition to be 1 or 0, got {}", done[cond])
                }
            }
            Op::Constant(c) => c.clone(),
            Op::Argument(arg) => args[&arg.name].clone(),
        };
        done.insert(Id::from(idx), result);
    }

    done[&Id::from(op.as_ref().len() - 1)].clone()
}
