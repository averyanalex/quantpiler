use crate::expression::Expression;

#[test]
fn crc32() {
    fn table(ch: Expression) -> Expression {
        let poly = 0xEDB8_8320_u32;

        (0..8u32)
            .map(|i| ch >> i)
            .fold(Expression::constant(0u32), |table, ch| {
                ((ch ^ table) & 1u32).ternary((table >> 1u32) ^ poly, table >> 1u32)
            })
    }

    let size = 32;

    let input = Expression::argument("input", size);
    let mut value = Expression::constant(0xFFFF_FFFF_u32);

    for byte in (0..(size / 8)).map(|i| (input >> (i * 8)) & 0xFFu32) {
        let ch = (byte ^ value) & 0xFFu32;
        value = table(ch) ^ (value >> 8u32);
    }

    value.compile();
}

#[test]
fn example() {
    let a = Expression::argument("input", 3);
    let b = Expression::argument("b", 2);
    let add = a + b;
    let xor = add ^ 0b100u32;
    let and_const = xor & 0b111u32;

    and_const.compile();
}

#[test]
fn apowxmodn() {
    let x_len = 3;
    let x = Expression::argument("x", 3);

    let a = 5u32;

    let prod = (0..x_len).fold(Expression::constant(1u32), |acc, i| {
        ((x >> i) & 1u32).ternary(acc * a.pow(2u32.pow(i)), acc) & 0b1111u32
    });

    prod.compile();
}

#[test]
fn add() {
    (Expression::argument("a", 32) + Expression::argument("b", 16)).compile();
}

#[test]
fn many_add() {
    (Expression::argument("a", 32)
        + Expression::argument("b", 20)
        + Expression::argument("c", 27)
        + Expression::argument("d", 10)
        + Expression::argument("e", 24))
    .compile();
}

#[test]
fn mul() {
    (Expression::argument("a", 16) * Expression::argument("b", 8)).compile();
}

#[test]
fn many_mul() {
    (Expression::argument("a", 10)
        * Expression::argument("b", 8)
        * Expression::argument("c", 2)
        * Expression::argument("d", 5))
    .compile();
}

#[test]
fn and() {
    (Expression::argument("a", 32) & Expression::argument("b", 16)).compile();
}

#[test]
fn many_and() {
    (Expression::argument("a", 32)
        & Expression::argument("b", 20)
        & Expression::argument("c", 27)
        & Expression::argument("d", 10)
        & Expression::argument("e", 24))
    .compile();
}

#[test]
fn or() {
    (Expression::argument("a", 32) | Expression::argument("b", 16)).compile();
}

#[test]
fn many_or() {
    (Expression::argument("a", 32)
        | Expression::argument("b", 20)
        | Expression::argument("c", 27)
        | Expression::argument("d", 10)
        | Expression::argument("e", 24))
    .compile();
}

#[test]
fn xor() {
    (Expression::argument("a", 32) ^ Expression::argument("b", 16)).compile();
}

#[test]
fn many_xor() {
    (Expression::argument("a", 32)
        ^ Expression::argument("b", 20)
        ^ Expression::argument("c", 27)
        ^ Expression::argument("d", 10)
        ^ Expression::argument("e", 24))
    .compile();
}

#[test]
fn rem_constant() {
    (Expression::argument("a", 32) % Expression::constant(15u64)).compile();
}

#[test]
fn rem_argument() {
    (Expression::argument("a", 8)
        % (Expression::argument("b", 3)
            + Expression::argument("c", 3)
            + Expression::constant(1u64)))
    .compile();
}

#[test]
fn eq_expr() {
    Expression::argument("a", 32)
        .eq(Expression::argument("b", 32))
        .compile();
}

#[test]
fn lt_expr() {
    Expression::argument("a", 32)
        .lt(Expression::argument("b", 32))
        .compile();
}

#[test]
fn gt_expr() {
    Expression::argument("a", 32)
        .gt(Expression::argument("b", 32))
        .compile();
}

#[test]
fn ne_expr() {
    Expression::argument("a", 32)
        .ne(Expression::argument("b", 32))
        .compile();
}

#[test]
fn ge_expr() {
    Expression::argument("a", 32)
        .ge(Expression::argument("b", 32))
        .compile();
}

#[test]
fn le_expr() {
    Expression::argument("a", 32)
        .le(Expression::argument("b", 32))
        .compile();
}
