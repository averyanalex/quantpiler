use egg::RecExpr;
use rand::Rng;
use rustc_hash::FxHashMap;

use crate::{circuit::Circuit, logic::Logic, op::Op};

fn vec_bool_to_biguint(input: &[bool]) -> num::BigUint {
    let mut res = <num::BigUint as num::Zero>::zero();
    for (idx, bit) in input.iter().enumerate() {
        if *bit {
            res += 2u128.pow(idx as u32);
        }
    }
    res
}

pub fn verify(expr: &RecExpr<Op>, logic: &RecExpr<Logic>, circuit: &Circuit) {
    let mut arguments = FxHashMap::default();
    for op in expr.as_ref() {
        if let Op::Argument(a) = op {
            assert!(arguments.insert(a.name.clone(), a.size).is_none());
        }
    }

    for _ in 0..8192 {
        let mut rng_bits_args = FxHashMap::default();
        let mut rng = rand::thread_rng();

        for (name, size) in &arguments {
            rng_bits_args.insert(
                name.clone(),
                std::iter::repeat_with(|| rng.gen())
                    .take(*size)
                    .collect::<Vec<_>>(),
            );
        }

        let rng_biguint_args: FxHashMap<_, _> = rng_bits_args
            .iter()
            .map(|(name, bits)| (name.clone(), vec_bool_to_biguint(bits.as_slice())))
            .collect();

        let op_result = crate::executor::execute_op(expr, &rng_biguint_args);
        let logic_result = crate::executor::execute_logic(logic, &rng_bits_args);
        let biguint_logic_result = vec_bool_to_biguint(&logic_result);

        assert_eq!(op_result, biguint_logic_result);

        let circuit_result = circuit.execute(&rng_bits_args);
        assert_eq!(logic_result, circuit_result);
    }
}
