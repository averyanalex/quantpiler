#![feature(let_chains)]

use itertools::Itertools;
use rand::prelude::*;
use rustc_hash::FxHashMap;

// use crate::frontend::Ast;

use std::iter;

use crate::frontend::Ast;

mod bitificator;
mod circuit;
mod compiler;
mod executor;
mod frontend;
// mod graph;
mod extract;
mod unwrapper;
// mod extract;

// fn main() {
//     let mut input = String::new(); // создаём строку для ввода
//     std::io::stdin()
//         .read_line(&mut input) // читаем в строку из стандартного ввода
//         .expect("failed to read line"); // если произошла ошибка - завершаем программу

//     let result: String = input
//         .chars() // получаем итератор символов исходной строки
//         // сейчас будет говнокод, ибо я хз, как ещё не маппить первый символ
//         .enumerate() // добавляем к символу его номер
//         .filter_map(|(index, char)| {
//             if index == 0 {
//                 Some(char) // с первым символом ничего не делаем
//             } else { // над остальными издеваемся
//                 match char {
//                     // маппим итератор с фильтрацией - или берём одно и даём другое, либо пропускаем
//                     // берём исходный символ и преобразуем в цифру или пропускаем
//                     'b' | 'f' | 'p' | 'v' => Some('1'),
//                     'c' | 'g' | 'j' | 'k' | 'q' | 's' | 'x' | 'z' => Some('2'),
//                     'd' | 't' => Some('3'),
//                     'l' => Some('4'),
//                     'm' | 'n' => Some('5'),
//                     'r' => Some('6'),
//                     _ => None, // говорим, что это надо пропустить
//                 }
//             }
//         })
//         .dedup() // убираем подряд идущие одинаковые элементы (требуется itertools)
//         .chain(std::iter::repeat('0')) // если нормальные данные кончились, подсовываем нули
//         .take(4) // не берём более 4 символов (т.к. выше делаем бесконечно нулей, то как раз 4 и возьмём)
//         .collect(); // собираем итератор символов в строку

//     println!("{}", result); // выводим результат
// }

fn main() {
    tracing_subscriber::fmt::init();

    let ast = frontend::parse();
    println!("AST Done");

    let args_list = if let Ast::Function {
        name: _,
        arguments,
        instructions: _,
    } = &ast
    {
        arguments.clone()
    } else {
        panic!()
    };

    let op = unwrapper::Unwrapper::new().unwrap(ast);
    // let op = unwrapper::unwrap_func(ast.clone());
    println!("Unwrap done");

    let logic = bitificator::Bitificator::new(op).bitificate();
    println!("Bitificate done");

    let circuit = compiler::Compiler::new(&logic, args_list).compile();
    // dbg!(&circuit);

    for _ in 0..4096 {
        let mut args = FxHashMap::default();
        let mut rng = rand::thread_rng();
        args.insert(
            "a".into(),
            iter::repeat_with(|| rng.gen()).take(8).collect_vec(),
        );
        args.insert(
            "b".into(),
            iter::repeat_with(|| rng.gen()).take(8).collect_vec(),
        );
        args.insert(
            "c".into(),
            iter::repeat_with(|| rng.gen()).take(8).collect_vec(),
        );
        args.insert(
            "d".into(),
            iter::repeat_with(|| rng.gen()).take(8).collect_vec(),
        );

        let bits_result = executor::execute_gates(&logic, &args);
        let circuit_result = circuit.execute(&args);
        assert_eq!(bits_result, circuit_result);
    }

    let mut args = FxHashMap::default();
    args.insert("a".into(), vec![false; 8]);
    args.insert("b".into(), vec![false; 8]);
    args.insert("c".into(), vec![false; 8]);
    args.insert("d".into(), vec![false; 8]);

    let bits_result = executor::execute_gates(&logic, &args);
    assert_eq!(
        bits_result,
        vec![
            true, true, false, false, false, true, true, true, false, false, false, false, false,
            true, false, false, true, true, false, true, true, true, false, true, false, true,
            true, true, true, false, true, true
        ]
    );
}
