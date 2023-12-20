use crate::{compile, expression::Expression};

#[test]
fn crc32() {
    let size = 32;

    let input = Expression::new_argument("input", size);
    let mut value = input.constant(0xFFFFFFFFu32);

    fn table(mut ch: Expression) -> Expression {
        let poly = 0xEDB88320u32;

        let mut table = ch.constant(0u32);
        for _ in 0..8u32 {
            table = ((ch.clone() ^ table.clone()) & 1u32)
                .ternary((table.clone() >> 1u32) ^ poly, table >> 1u32);
            ch = ch >> 1u32;
        }
        table
    }

    for byte in (0..(size / 8)).map(|i| (input.clone() >> (i * 8)) & 0xFFu32) {
        let ch = (byte ^ value.clone()) & 0xFFu32;
        value = table(ch) ^ (value >> 8u32);
    }

    compile(&value);
}

#[test]
fn example() {
    let a = Expression::new_argument("input", 3);
    let b = a.argument("b", 2);
    let add = a + b;
    let xor = add ^ 0b100u32;
    let and_const = xor & 0b111u32;

    compile(&and_const);
}

#[test]
fn add() {
    let a = Expression::new_argument("a", 32);
    compile(&(a.clone() + a.argument("b", 16)));
}

#[test]
fn many_add() {
    let a = Expression::new_argument("a", 32);
    compile(
        &(a.clone()
            + a.argument("b", 20)
            + a.argument("c", 27)
            + a.argument("d", 10)
            + a.argument("e", 24)),
    );
}

#[test]
fn mul() {
    let a = Expression::new_argument("a", 16);
    compile(&(a.clone() * a.argument("b", 8)));
}

#[test]
fn many_mul() {
    let a = Expression::new_argument("a", 16);
    compile(&(a.clone() * a.argument("b", 8) * a.argument("c", 2) * a.argument("d", 5)));
}

#[test]
fn and() {
    let a = Expression::new_argument("a", 32);
    compile(&(a.clone() & a.argument("b", 16)));
}

#[test]
fn many_and() {
    let a = Expression::new_argument("a", 32);
    compile(
        &(a.clone()
            & a.argument("b", 20)
            & a.argument("c", 27)
            & a.argument("d", 10)
            & a.argument("e", 24)),
    );
}

#[test]
fn or() {
    let a = Expression::new_argument("a", 32);
    compile(&(a.clone() | a.argument("b", 16)));
}

#[test]
fn many_or() {
    let a = Expression::new_argument("a", 32);
    compile(
        &(a.clone()
            | a.argument("b", 20)
            | a.argument("c", 27)
            | a.argument("d", 10)
            | a.argument("e", 24)),
    );
}

#[test]
fn xor() {
    let a = Expression::new_argument("a", 32);
    compile(&(a.clone() ^ a.argument("b", 16)));
}

#[test]
fn many_xor() {
    let a = Expression::new_argument("a", 32);
    compile(
        &(a.clone()
            ^ a.argument("b", 20)
            ^ a.argument("c", 27)
            ^ a.argument("d", 10)
            ^ a.argument("e", 24)),
    );
}
