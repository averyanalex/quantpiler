use std::rc::Rc;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::frontend::Ast;

#[derive(Debug, PartialEq, Eq)]
pub enum Op {
    Argument {
        size: u32,
        name: String,
    },
    Ternary {
        condition: Rc<Self>,
        then: Rc<Self>,
        or: Rc<Self>,
    },
    Constant(u32),
    Index {
        index: Rc<Self>,
        target: Rc<Self>,
    },
    IndexRange {
        from: Rc<Self>,
        to: Rc<Self>,
        target: Rc<Self>,
    },
    Not(Rc<Self>),
    Xor(FxHashSet<Rc<Self>>),
    Or(FxHashSet<Rc<Self>>),
    And(FxHashSet<Rc<Self>>),
    // Multiplication(Rc<Self>, Rc<Self>),
    // Sum(Rc<Self>, Rc<Self>),
    RShift {
        target: Rc<Self>,
        distance: Rc<Self>,
    },
    Equal(FxHashSet<Rc<Self>>),
}

impl std::hash::Hash for Op {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // Derived hash is too expensive
        let id = match self {
            Op::Argument { .. } => 0,
            Op::Ternary {
                condition: _,
                then: _,
                or: _,
            } => 1,
            Op::Constant(_) => 2,
            Op::Index {
                index: _,
                target: _,
            } => 3,
            Op::IndexRange {
                from: _,
                to: _,
                target: _,
            } => 4,
            Op::Not(_) => 5,
            Op::Xor(_) => 6,
            Op::Or(_) => 7,
            Op::And(_) => 8,
            Op::RShift {
                target: _,
                distance: _,
            } => 9,
            Op::Equal(_) => 10,
        };
        state.write_u8(id);
    }
}

fn unwrap_op(
    vars: &FxHashMap<String, Rc<Op>>,
    dedup_cache: &mut FxHashSet<Rc<Op>>,
    ast_cache: &mut FxHashMap<Ast, Rc<Op>>,
    ast: &Ast,
) -> Rc<Op> {
    let op = match ast_cache.get(ast) {
        Some(op) => {
            return op.clone();
        }
        None => match ast {
            Ast::Variable(variable) => return vars[variable].clone(),
            Ast::Xor(args) => Op::Xor(
                args.iter()
                    .map(|arg| unwrap_op(vars, dedup_cache, ast_cache, arg))
                    .collect(),
            ),
            Ast::Or(args) => Op::Or(
                args.iter()
                    .map(|arg| unwrap_op(vars, dedup_cache, ast_cache, arg))
                    .collect(),
            ),
            Ast::And(args) => Op::And(
                args.iter()
                    .map(|arg| unwrap_op(vars, dedup_cache, ast_cache, arg))
                    .collect(),
            ),
            Ast::Not(arg) => Op::Not(unwrap_op(vars, dedup_cache, ast_cache, arg)),
            Ast::Ternary {
                condition,
                then,
                or,
            } => Op::Ternary {
                condition: unwrap_op(vars, dedup_cache, ast_cache, condition),
                then: unwrap_op(vars, dedup_cache, ast_cache, then),
                or: unwrap_op(vars, dedup_cache, ast_cache, or),
            },
            Ast::Index { index, target } => Op::Index {
                index: unwrap_op(vars, dedup_cache, ast_cache, index),
                target: unwrap_op(vars, dedup_cache, ast_cache, target),
            },
            Ast::IndexRange { from, to, target } => Op::IndexRange {
                from: unwrap_op(vars, dedup_cache, ast_cache, from),
                to: unwrap_op(vars, dedup_cache, ast_cache, to),
                target: unwrap_op(vars, dedup_cache, ast_cache, target),
            },
            Ast::Constant(value) => Op::Constant(*value),
            Ast::RShift { target, distance } => Op::RShift {
                target: unwrap_op(vars, dedup_cache, ast_cache, target),
                distance: unwrap_op(vars, dedup_cache, ast_cache, distance),
            },
            Ast::Equal(args) => Op::Equal(
                args.iter()
                    .map(|arg| unwrap_op(vars, dedup_cache, ast_cache, arg))
                    .collect(),
            ),
            _ => todo!("{:?}", ast),
        },
    };

    let op = match dedup_cache.get(&op) {
        Some(op) => op.clone(),
        None => {
            let op = Rc::new(op);
            dedup_cache.insert(op.clone());
            op
        }
    };
    ast_cache.insert(ast.clone(), op.clone());

    op
}

fn unwrap_instructions(
    instructions: Vec<Ast>,
    vars: &mut FxHashMap<String, Rc<Op>>,
    dedup_cache: &mut FxHashSet<Rc<Op>>,
    ast_cache: &mut FxHashMap<Ast, Rc<Op>>,
) -> Option<Rc<Op>> {
    for inst in instructions {
        match inst {
            Ast::Assignment { variable, value } => {
                let op = unwrap_op(vars, dedup_cache, ast_cache, &value);
                vars.insert(variable, op);
                ast_cache.clear();
            }
            Ast::Return(value) => return Some(unwrap_op(vars, dedup_cache, ast_cache, &value)),
            Ast::StaticForLoop {
                variable,
                values,
                instructions,
            } => {
                for value in values {
                    vars.insert(
                        variable.clone(),
                        unwrap_op(vars, dedup_cache, ast_cache, &value),
                    );
                    ast_cache.clear();
                    if let Some(ret) =
                        unwrap_instructions(instructions.clone(), vars, dedup_cache, ast_cache)
                    {
                        return Some(ret);
                    };
                }
            }
            _ => panic!(),
        }
    }

    None
}

pub fn unwrap_func(ast: Ast) -> Rc<Op> {
    let Ast::Function { name: _, arguments, instructions } = ast else {todo!()};

    let mut vars = FxHashMap::default();
    for (name, size) in arguments {
        vars.insert(name.clone(), Rc::new(Op::Argument { name, size }));
    }

    let mut dedup_cache = FxHashSet::default();
    let mut ast_cache = FxHashMap::default();

    unwrap_instructions(instructions, &mut vars, &mut dedup_cache, &mut ast_cache).unwrap()
}
