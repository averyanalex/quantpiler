use std::rc::Rc;

use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::unwrapper::Op;

#[derive(Debug, PartialEq, Eq)]
pub enum Gate {
    Argument { name: String, index: u32 },
    Constant(bool),
    Not(Rc<Gate>),
    And(FxHashSet<Rc<Self>>),
    Or(FxHashSet<Rc<Self>>),
    Xor(FxHashSet<Rc<Self>>),
}

impl std::hash::Hash for Gate {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // Derived hash is too expensive
        let id = match self {
            Gate::Argument { name: _, index: _ } => 0,
            Gate::Constant(_) => 1,
            Gate::Not(_) => 2,
            Gate::And(_) => 3,
            Gate::Or(_) => 4,
            Gate::Xor(_) => 5,
        };
        state.write_u8(id);
    }
}

impl Gate {
    pub const fn is_const_true(&self) -> bool {
        match self {
            Self::Constant(value) => *value,
            _ => false,
        }
    }

    pub fn is_const_false(&self) -> bool {
        match self {
            Self::Constant(value) => !value,
            _ => false,
        }
    }

    pub fn argument(name: String, index: u32, dedup_cache: &mut FxHashSet<Rc<Gate>>) -> Rc<Self> {
        dedup_gate(Self::Argument { name, index }, dedup_cache)
    }

    pub fn constant(value: bool, dedup_cache: &mut FxHashSet<Rc<Gate>>) -> Rc<Self> {
        dedup_gate(Self::Constant(value), dedup_cache)
    }

    pub fn not(bit: Rc<Gate>, dedup_cache: &mut FxHashSet<Rc<Gate>>) -> Rc<Self> {
        match bit.as_ref() {
            Self::Constant(value) => Self::constant(!value, dedup_cache),
            Self::Not(bit_in_not) => bit_in_not.clone(),
            _ => dedup_gate(Self::Not(bit), dedup_cache),
        }
    }

    pub fn and(bits: &[Rc<Gate>], dedup_cache: &mut FxHashSet<Rc<Gate>>) -> Rc<Self> {
        if bits.iter().any(|b| b.is_const_false()) {
            Self::constant(false, dedup_cache)
        } else if bits.len() == 1 {
            bits[0].clone()
        } else {
            dedup_gate(
                Self::And(
                    bits.iter()
                        .filter(|b| !b.is_const_true())
                        .cloned()
                        .collect(),
                ),
                dedup_cache,
            )
        }
    }

    pub fn or(bits: &[Rc<Gate>], dedup_cache: &mut FxHashSet<Rc<Gate>>) -> Rc<Self> {
        if bits.iter().any(|b| b.is_const_true()) {
            Self::constant(true, dedup_cache)
        } else if bits.len() == 1 {
            bits[0].clone()
        } else {
            dedup_gate(
                Self::Or(
                    bits.iter()
                        .filter(|b| !b.is_const_false())
                        .cloned()
                        .collect(),
                ),
                dedup_cache,
            )
        }
    }

    pub fn xor(bits: &[Rc<Gate>], dedup_cache: &mut FxHashSet<Rc<Gate>>) -> Rc<Self> {
        let bits: FxHashSet<_> = bits
            .iter()
            .filter(|b| !b.is_const_false()) // false in XOR does nothing
            .counts()
            .into_iter()
            .filter_map(|(b, count)| {
                if count % 2 == 1 {
                    Some(b.clone())
                } else {
                    None // pairs of identical gates are mutually destroyed
                }
            })
            .collect();

        if bits.is_empty() {
            Self::constant(false, dedup_cache)
        } else if bits.len() == 1 {
            bits.iter().next().unwrap().clone()
        } else if bits.iter().any(|b| b.is_const_true()) {
            let bits_no_true: FxHashSet<_> =
                bits.into_iter().filter(|b| !b.is_const_true()).collect();

            let xor_without_true = if bits_no_true.is_empty() {
                Self::constant(false, dedup_cache)
            } else if bits_no_true.len() == 1 {
                bits_no_true.iter().next().unwrap().clone()
            } else {
                dedup_gate(Self::Xor(bits_no_true), dedup_cache)
            };

            Self::not(xor_without_true, dedup_cache)
        } else {
            dedup_gate(Self::Xor(bits), dedup_cache)
        }
    }
}

fn dedup_gate(gate: Gate, dedup_cache: &mut FxHashSet<Rc<Gate>>) -> Rc<Gate> {
    match dedup_cache.get(&gate) {
        Some(existing_gate) => existing_gate.clone(),
        None => {
            let gate = Rc::new(gate);
            dedup_cache.insert(gate.clone());
            gate
        }
    }
}

fn columns_of_gates<'a, T: IntoIterator<Item = &'a Rc<Op>>>(
    args: T,
    op_cache: &mut FxHashMap<Rc<Op>, Rc<Vec<Rc<Gate>>>>,
    gates_dedup_cache: &mut FxHashSet<Rc<Gate>>,
) -> Vec<Vec<Rc<Gate>>> {
    let executed_args = args
        .into_iter()
        .map(|arg| bitificate_op_rec(arg, op_cache, gates_dedup_cache))
        .collect_vec();
    let max_len = executed_args.iter().map(|a| a.len()).max().unwrap();
    (0..max_len)
        .map(|index| {
            executed_args
                .iter()
                .filter_map(|arg| arg.get(index).cloned())
                .collect_vec()
        })
        .collect_vec()
}

pub fn bitificate_op_rec(
    op: &Rc<Op>,
    op_cache: &mut FxHashMap<Rc<Op>, Rc<Vec<Rc<Gate>>>>,
    gates_dedup_cache: &mut FxHashSet<Rc<Gate>>,
) -> Rc<Vec<Rc<Gate>>> {
    match op_cache.get(op) {
        Some(gates) => gates.clone(),
        None => {
            let gates: Vec<Rc<Gate>> = match op.as_ref() {
                Op::Argument { size, name } => (0..*size)
                    .map(|i| Gate::argument(name.clone(), i, gates_dedup_cache))
                    .collect(),
                Op::Ternary {
                    condition,
                    then,
                    or,
                } => {
                    let condition_bit = bitificate_op_rec(condition, op_cache, gates_dedup_cache)
                        .get(0)
                        .unwrap()
                        .clone();
                    let then = bitificate_op_rec(then, op_cache, gates_dedup_cache)
                        .iter()
                        .map(|g| Gate::and(&[g.clone(), condition_bit.clone()], gates_dedup_cache))
                        .collect_vec();

                    let not_condition_bit = Gate::not(condition_bit, gates_dedup_cache);
                    let or = bitificate_op_rec(or, op_cache, gates_dedup_cache)
                        .iter()
                        .map(|g| {
                            Gate::and(&[g.clone(), not_condition_bit.clone()], gates_dedup_cache)
                        })
                        .collect_vec();

                    let max_len = then.len().max(or.len());
                    (0..max_len)
                        .map(|index| {
                            [&then, &or]
                                .into_iter()
                                .filter_map(|it| it.get(index).cloned())
                                .collect_vec()
                        })
                        .map(|gates| Gate::xor(&gates[..], gates_dedup_cache))
                        .collect()
                }
                Op::Constant(value) => (0..32)
                    .map(|i| ((value >> i) & 1) == 1)
                    .map(|b| Gate::constant(b, gates_dedup_cache))
                    .collect(),
                Op::Index { index, target } => {
                    let Op::Constant(index) = index.as_ref() else {todo!()};
                    vec![bitificate_op_rec(target, op_cache, gates_dedup_cache)
                        .get(*index as usize)
                        .unwrap()
                        .clone()]
                }
                // Op::IndexRange { from, to, target } => todo!(),
                Op::Not(arg) => bitificate_op_rec(arg, op_cache, gates_dedup_cache)
                    .iter()
                    .map(|g| Gate::not(g.clone(), gates_dedup_cache))
                    .collect(),
                Op::Xor(args) => columns_of_gates(args, op_cache, gates_dedup_cache)
                    .into_iter()
                    .map(|column| Gate::xor(&column, gates_dedup_cache))
                    .collect(),
                Op::Or(args) => columns_of_gates(args, op_cache, gates_dedup_cache)
                    .into_iter()
                    .map(|column| Gate::or(&column, gates_dedup_cache))
                    .collect(),
                Op::And(args) => columns_of_gates(args, op_cache, gates_dedup_cache)
                    .into_iter()
                    .map(|column| Gate::and(&column, gates_dedup_cache))
                    .collect(),
                // Op::Multiplication(_, _) => todo!(),
                // Op::Sum(_, _) => todo!(),
                Op::RShift { target, distance } => {
                    let Op::Constant(distance) = distance.as_ref() else {todo!()};
                    bitificate_op_rec(target, op_cache, gates_dedup_cache)
                        .iter()
                        .skip(*distance as usize)
                        .cloned()
                        .collect()
                }
                _ => todo!(),
            };

            // let gates = gates
            //     .into_iter()
            //     .rev()
            //     .skip_while(|g| g.is_const_false())
            //     .collect_vec()
            //     .into_iter()
            //     .rev()
            //     .collect_vec();

            let rc_gates = Rc::new(gates);
            op_cache.insert(op.clone(), rc_gates.clone());
            rc_gates
        }
    }
}

pub fn bitificate_op(op: &Rc<Op>) -> Vec<Rc<Gate>> {
    let mut op_cache = FxHashMap::default();
    let mut gates_dedup_cache = FxHashSet::default();
    bitificate_op_rec(op, &mut op_cache, &mut gates_dedup_cache)
        .iter()
        .cloned()
        .collect()
}
