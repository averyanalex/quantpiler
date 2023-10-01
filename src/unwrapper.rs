use std::{fmt::Display, str::FromStr};

use egg::*;

use num::BigUint;
use rustc_hash::FxHashMap;

use crate::frontend::Ast;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ArgumentInfo {
    pub size: u32,
    pub name: String,
}

impl Display for ArgumentInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({})", self.name, self.size)
    }
}

impl FromStr for ArgumentInfo {
    type Err = ();

    fn from_str(_s: &str) -> Result<Self, Self::Err> {
        Err(())
    }
}

define_language! {
    pub enum Op {
        "!" = Not(Id),
        "^" = Xor([Id; 2]),
        "|" = Or([Id; 2]),
        "&" = And([Id; 2]),
        ">>" = RShift([Id; 2]),
        "<<" = LShift([Id; 2]),
        "+" = Add([Id; 2]),
        "-" = Sub([Id; 2]),
        "*" = Mul([Id; 2]),
        "/" = Div([Id; 2]),
        "==" = Eq([Id; 2]),
        "?" = Ternary([Id; 3]),
        "i" = Index([Id; 2]),
        "ir" = IndexRange([Id; 3]),
        Constant(BigUint),
        Argument(ArgumentInfo),
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct AnalyzerData {
    value: Option<BigUint>,
}

#[derive(Default)]
struct OpAnalyzer;
impl Analysis<Op> for OpAnalyzer {
    type Data = AnalyzerData;

    fn merge(&mut self, to: &mut Self::Data, from: Self::Data) -> DidMerge {
        egg::merge_max(to, from)
    }

    fn make(egraph: &EGraph<Op, Self>, enode: &Op) -> Self::Data {
        let make_value = || {
            let x = |i: &Id| egraph[*i].data.value.clone();
            match enode {
                Op::Xor([a, b]) => Some(x(a)? ^ x(b)?),
                Op::Or([a, b]) => Some(x(a)? | x(b)?),
                Op::And([a, b]) => Some(x(a)? & x(b)?),
                Op::RShift([target, distance]) => {
                    Some(x(target)? >> u128::try_from(x(distance)?).unwrap())
                }
                _ => None,
            }
        };

        let value = make_value();

        Self::Data { value }
    }

    fn modify(egraph: &mut EGraph<Op, Self>, id: Id) {
        if let Some(i) = &egraph[id].data.value {
            let added = egraph.add(Op::Constant(i.clone()));
            egraph.union(id, added);
        }
    }
}

fn make_rules() -> Vec<Rewrite<Op, OpAnalyzer>> {
    use egg::rewrite as rw;
    vec![
        // Commutable
        rw!("comm-xor"; "(^ ?a ?b)" => "(^ ?b ?a)"),
        rw!("comm-and"; "(& ?a ?b)" => "(& ?b ?a)"),
        // Associative
        rw!("assoc-xor"; "(^ ?a (^ ?b ?c))" => "(^ (^ ?a ?b) ?c)"),
        rw!("assoc-and"; "(& ?a (& ?b ?c))" => "(& (& ?a ?b) ?c)"),
        // Same elements logic
        rw!("and-same"; "(& ?a ?a)" => "?a"),
        rw!("and-same-not"; "(& (! ?a) ?a)" => "0"),
        rw!("xor-same"; "(^ ?a ?a)" => "0"),
        // Etc
        rw!("cancel-not"; "(! (! ?a))" => "?a"),
        rw!("xor-not-not-xor"; "(^ (! ?a) ?b)" => "(! (^ ?a ?b))"),
        rw!("cancel-xor-not-not"; "(^ (! ?a) (! ?b))" => "(^ ?a ?b)"),
        rw!("not-xor-xor-not"; "(! (^ ?a ?b))" => "(^ (! ?a) ?b)"),
    ]
}

pub struct Unwrapper {
    variables: FxHashMap<String, Id>,
    egraph: EGraph<Op, OpAnalyzer>,
}

impl Unwrapper {
    pub fn new() -> Self {
        Self {
            variables: FxHashMap::default(),
            egraph: EGraph::new(OpAnalyzer),
        }
    }

    pub fn unwrap_ast(&mut self, ast: &Ast) -> Id {
        let op = match ast {
            Ast::Variable(name) => return self.variables[name],
            Ast::Xor(a, b) => Op::Xor([self.unwrap_ast(a), self.unwrap_ast(b)]),
            Ast::Or(a, b) => Op::Or([self.unwrap_ast(a), self.unwrap_ast(b)]),
            Ast::And(a, b) => Op::And([self.unwrap_ast(a), self.unwrap_ast(b)]),
            Ast::Ternary {
                condition,
                then,
                or,
            } => Op::Ternary([
                self.unwrap_ast(condition),
                self.unwrap_ast(then),
                self.unwrap_ast(or),
            ]),
            Ast::RShift { target, distance } => {
                Op::RShift([self.unwrap_ast(target), self.unwrap_ast(distance)])
            }
            Ast::Constant(value) => Op::Constant(value.clone()),
            _ => todo!("{:?}", ast),
        };

        self.egraph.add(op)
    }

    pub fn unwrap_instructions(&mut self, instructions: Vec<Ast>) -> Option<Id> {
        for inst in instructions {
            match inst {
                Ast::Assignment { variable, value } => {
                    let id = self.unwrap_ast(&value);
                    self.variables.insert(variable, id);
                }
                Ast::Return(value) => return Some(self.unwrap_ast(&value)),
                Ast::StaticForLoop {
                    variable,
                    values,
                    instructions,
                } => {
                    for value in values {
                        let id = self.unwrap_ast(&value);
                        self.variables.insert(variable.clone(), id);
                        if let Some(ret) = self.unwrap_instructions(instructions.clone()) {
                            return Some(ret);
                        };
                    }
                }
                _ => panic!(),
            }
        }

        None
    }

    pub fn unwrap(mut self, ast: Ast) -> RecExpr<Op> {
        let Ast::Function { arguments, instructions, .. } = ast else {todo!()};

        for (name, size) in arguments {
            let var = Op::Argument(ArgumentInfo {
                size,
                name: name.clone(),
            });
            let id = self.egraph.add(var);
            self.variables.insert(name, id);
        }

        let root = self.unwrap_instructions(instructions).unwrap();

        let mut runner = Runner::default()
            .with_egraph(self.egraph)
            .with_time_limit(std::time::Duration::from_secs(3600))
            .with_node_limit(100_000)
            .with_iter_limit(50);
        runner.roots.push(root);

        runner = runner.run(&make_rules());

        crate::extract::extract(&runner.egraph, runner.roots[0], AstSize)
    }
}
