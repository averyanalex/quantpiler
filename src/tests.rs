use num::{BigUint, Zero};
use rand::Rng;
use rustc_hash::FxHashMap;

use crate::builder::{Expression, Op, OpBuilder};

fn vec_bool_to_biguint(input: &[bool]) -> BigUint {
    let mut res = BigUint::zero();
    for (idx, bit) in input.iter().enumerate() {
        if *bit {
            res += 2u128.pow(idx as u32);
        }
    }
    res
}

fn test_expr(builder: &OpBuilder, value: Expression<'_>) {
    let expr = builder.build(value);

    let mut arguments = FxHashMap::default();
    for op in expr.as_ref() {
        if let Op::Argument(a) = op {
            assert!(arguments.insert(a.name.clone(), a.size).is_none())
        }
    }

    let logic = crate::logic::Logificator::new(expr.clone()).build_logic();
    dbg!(&logic);

    let circuit = crate::compiler::Compiler::new(&logic).compile();

    for _ in 0..4096 {
        let mut rng_bits_args = FxHashMap::default();
        let mut rng = rand::thread_rng();

        for (name, size) in &arguments {
            rng_bits_args.insert(
                name.clone(),
                std::iter::repeat_with(|| rng.gen())
                    .take(*size as usize)
                    .collect::<Vec<_>>(),
            );
        }

        let rng_biguint_args: FxHashMap<_, _> = rng_bits_args
            .iter()
            .map(|(name, bits)| (name.clone(), vec_bool_to_biguint(bits.as_slice())))
            .collect();

        let op_result = crate::executor::execute_op(&expr, &rng_biguint_args);
        let logic_result = crate::executor::execute_logic(&logic, &rng_bits_args);
        let biguint_logic_result = vec_bool_to_biguint(&logic_result);

        assert_eq!(op_result, biguint_logic_result);

        let circuit_result = circuit.execute(&rng_bits_args);
        assert_eq!(logic_result, circuit_result);
    }
}

#[test]
fn crc32() {
    let builder = crate::builder::OpBuilder::default();
    let mut value = builder.constant(0xFFFFFFFFu32);
    let size = 32;
    let input = builder.argument("input", size);

    fn table<'a>(builder: &'a OpBuilder, mut ch: Expression<'a>) -> Expression<'a> {
        let poly = 0xEDB88320u32;

        let mut table = builder.constant(0u32);
        for _ in 0..8u32 {
            table = builder.ternary((ch ^ table) & 1u32, (table >> 1u32) ^ poly, table >> 1u32);
            ch = ch >> 1u32;
        }
        table
    }

    for byte in (0..(size / 8)).map(|i| (input >> (i * 8)) & 0xFFu32) {
        let ch = (byte ^ value) & 0xFFu32;
        value = table(&builder, ch) ^ (value >> 8u32);
    }

    test_expr(&builder, value);
}

#[test]
fn add() {
    let builder = crate::builder::OpBuilder::default();
    test_expr(
        &builder,
        builder.argument("a", 32) + builder.argument("b", 16),
    );
}

#[test]
fn many_add() {
    let builder = crate::builder::OpBuilder::default();
    test_expr(
        &builder,
        builder.argument("a", 32)
            + builder.argument("b", 20)
            + builder.argument("c", 27)
            + builder.argument("d", 10)
            + builder.argument("e", 24),
    );
}

#[test]
fn and() {
    let builder = crate::builder::OpBuilder::default();
    test_expr(
        &builder,
        builder.argument("a", 32) & builder.argument("b", 16),
    );
}

#[test]
fn many_and() {
    let builder = crate::builder::OpBuilder::default();
    test_expr(
        &builder,
        builder.argument("a", 32)
            & builder.argument("b", 20)
            & builder.argument("c", 27)
            & builder.argument("d", 10)
            & builder.argument("e", 24),
    );
}

#[test]
fn or() {
    let builder = crate::builder::OpBuilder::default();
    test_expr(
        &builder,
        builder.argument("a", 32) | builder.argument("b", 16),
    );
}

#[test]
fn many_or() {
    let builder = crate::builder::OpBuilder::default();
    test_expr(
        &builder,
        builder.argument("a", 32)
            | builder.argument("b", 20)
            | builder.argument("c", 27)
            | builder.argument("d", 10)
            | builder.argument("e", 24),
    );
}

#[test]
fn xor() {
    let builder = crate::builder::OpBuilder::default();
    test_expr(
        &builder,
        builder.argument("a", 32) ^ builder.argument("b", 16),
    );
}

#[test]
fn many_xor() {
    let builder = crate::builder::OpBuilder::default();
    test_expr(
        &builder,
        builder.argument("a", 32)
            ^ builder.argument("b", 20)
            ^ builder.argument("c", 27)
            ^ builder.argument("d", 10)
            ^ builder.argument("e", 24),
    );
}
