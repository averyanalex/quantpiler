use std::{cell::RefCell, fmt::Display, str::FromStr};

use egg::*;

use num::BigUint;

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

#[derive(Debug, Clone, Default)]
pub struct OpBuilder {
    egraph: RefCell<EGraph<Op, OpAnalyzer>>,
}

impl OpBuilder {
    pub fn build(&self, expr: Expression<'_>) -> RecExpr<Op> {
        let mut runner = Runner::default()
            .with_egraph(self.egraph.borrow().clone())
            .with_time_limit(std::time::Duration::from_secs(3600))
            .with_node_limit(100_000)
            .with_iter_limit(50);
        runner.roots.push(expr.id);

        runner = runner.run(&make_rules());

        crate::extract::extract(&runner.egraph, runner.roots[0], AstSize)
    }

    pub fn argument<N: Into<String>>(&self, name: N, size: u32) -> Expression {
        let id = self.egraph.borrow_mut().add(Op::Argument(ArgumentInfo {
            size,
            name: name.into(),
        }));
        Expression {
            id,
            egraph: &self.egraph,
        }
    }

    pub fn constant<T: Into<BigUint>>(&self, value: T) -> Expression {
        let id = self.egraph.borrow_mut().add(Op::Constant(value.into()));
        Expression {
            id,
            egraph: &self.egraph,
        }
    }

    pub fn ternary<T: IntoId>(&self, r#if: T, then: T, r#else: T) -> Expression {
        let if_id = r#if.id(&mut self.egraph.borrow_mut());
        let then_id = then.id(&mut self.egraph.borrow_mut());
        let else_id = r#else.id(&mut self.egraph.borrow_mut());

        let id = self
            .egraph
            .borrow_mut()
            .add(Op::Ternary([if_id, then_id, else_id]));
        Expression {
            id,
            egraph: &self.egraph,
        }
    }
}

pub trait IntoId {
    fn id(&self, egraph: &mut EGraph<Op, OpAnalyzer>) -> Id;
}

#[derive(Clone, Copy)]
pub struct Expression<'a> {
    id: Id,
    egraph: &'a RefCell<EGraph<Op, OpAnalyzer>>,
}

impl IntoId for Expression<'_> {
    fn id(&self, _egraph: &mut EGraph<Op, OpAnalyzer>) -> Id {
        self.id
    }
}

impl<T: Into<BigUint> + Clone> IntoId for T {
    fn id(&self, egraph: &mut EGraph<Op, OpAnalyzer>) -> Id {
        egraph.add(Op::Constant((*self).clone().into()))
    }
}

impl<'a> std::ops::Not for Expression<'a> {
    type Output = Self;

    fn not(self) -> Self::Output {
        let id = self.egraph.borrow_mut().add(Op::Not(self.id));
        Expression {
            id,
            egraph: self.egraph,
        }
    }
}

macro_rules! impl_op {
    ($std_op:ident, $op_func:ident, $lang_op:ident) => {
        impl<'a, T: IntoId> std::ops::$std_op<T> for Expression<'a> {
            type Output = Self;

            fn $op_func(self, rhs: T) -> Self::Output {
                let rhs_id = rhs.id(&mut self.egraph.borrow_mut());
                let id = self
                    .egraph
                    .borrow_mut()
                    .add(Op::$lang_op([self.id, rhs_id]));
                Expression {
                    id,
                    egraph: self.egraph,
                }
            }
        }
    };
}

impl_op! {BitXor, bitxor, Xor}
impl_op! {BitOr, bitor, Or}
impl_op! {BitAnd, bitand, And}

impl_op! {Shr, shr, Shr}
impl_op! {Shl, shl, Shl}

impl_op! {Add, add, Add}
impl_op! {Sub, sub, Sub}

impl_op! {Mul, mul, Mul}
impl_op! {Div, div, Div}
