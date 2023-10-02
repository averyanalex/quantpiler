use rand::Rng;
use rustc_hash::FxHashMap;

use crate::builder::{Expression, OpBuilder};

fn test_expr(builder: &OpBuilder, value: Expression<'_>, size: u32) {
    let expr = builder.build(value);
    let logic = crate::logic::Logificator::new(expr).build_logic();
    let circuit = crate::compiler::Compiler::new(&logic).compile();

    for _ in 0..1024 {
        let mut args = FxHashMap::default();
        let mut rng = rand::thread_rng();
        args.insert(
            "input".into(),
            std::iter::repeat_with(|| rng.gen())
                .take(size as usize)
                .collect(),
        );

        let bits_result = crate::executor::execute_gates(&logic, &args);
        let circuit_result = circuit.execute(&args);
        assert_eq!(bits_result, circuit_result);
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

    test_expr(&builder, value, size);
}
