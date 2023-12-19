use std::{fmt::Display, str::FromStr};

use egg::{define_language, Analysis, DidMerge, EGraph, Id, Rewrite};
use num::BigUint;

use crate::extract::LpCostFunction;

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
        ">>" = Shr([Id; 2]),
        "<<" = Shl([Id; 2]),
        "+" = Add([Id; 2]),
        "-" = Sub([Id; 2]),
        "*" = Mul([Id; 2]),
        "/" = Div([Id; 2]),
        "==" = Eq([Id; 2]),
        "?" = Ternary([Id; 3]),
        Constant(BigUint),
        Argument(ArgumentInfo),
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct AnalyzerData {
    value: Option<BigUint>,
}

#[derive(Default, Clone)]
pub struct OpAnalyzer;
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
                Op::Shr([target, distance]) => {
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

pub fn make_rules() -> Vec<Rewrite<Op, OpAnalyzer>> {
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

pub struct OpCost;

impl LpCostFunction<Op, OpAnalyzer> for OpCost {
    fn node_cost(&mut self, _egraph: &EGraph<Op, OpAnalyzer>, _eclass: Id, enode: &Op) -> f64 {
        match enode {
            Op::Not(_) => 1.0,
            Op::Xor(_) => 4.0,
            Op::Or(_) => 8.0,
            Op::And(_) => 6.0,
            Op::Shr(_) => 0.5,
            Op::Shl(_) => 0.5,
            Op::Add(_) => 16.0,
            Op::Sub(_) => 16.0,
            Op::Mul(_) => 32.0,
            Op::Div(_) => 32.0,
            Op::Eq(_) => 32.0,
            Op::Ternary(_) => 8.0,
            Op::Constant(_) => 0.1,
            Op::Argument(_) => 0.1,
        }
    }
}
