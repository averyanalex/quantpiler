use crate::{compile, expression::Expression};

#[test]
fn crc32() {
    fn table(ch: &Expression) -> Expression {
        let poly = 0xEDB8_8320_u32;

        (0..8u32)
            .map(|i| ch.clone() >> i)
            .fold(ch.constant(0u32), |table, ch| {
                ((ch ^ table.clone()) & 1u32)
                    .ternary(&((table.clone() >> 1u32) ^ poly), &(table >> 1u32))
            })
    }

    let size = 32;

    let input = Expression::new_argument("input", size);
    let mut value = input.constant(0xFFFF_FFFF_u32);

    for byte in (0..(size / 8)).map(|i| (input.clone() >> (i * 8)) & 0xFFu32) {
        let ch = (byte ^ value.clone()) & 0xFFu32;
        value = table(&ch) ^ (value >> 8u32);
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
fn apowxmodn() {
    let x_len = 3;
    let x = Expression::new_argument("x", 3);

    let a = 5u32;

    let prod = (0..x_len).fold(x.constant(1u32), |acc, i| {
        ((x.clone() >> i) & 1u32).ternary(&(acc.clone() * a.pow(2u32.pow(i))), &acc) & 0b1111u32
    });

    compile(&prod);
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
