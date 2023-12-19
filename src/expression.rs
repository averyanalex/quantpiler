use std::sync::{Arc, Mutex};

use egg::{AstSize, EGraph, Id, RecExpr, Runner};
use num::BigUint;
#[cfg(feature = "python")]
use pyo3::prelude::*;

#[cfg(feature = "python")]
use crate::circuit::Circuit;
use crate::op::{make_rules, ArgumentInfo, Op, OpAnalyzer};

#[derive(Clone)]
pub struct Expression {
    id: Id,
    egraph: Arc<Mutex<EGraph<Op, OpAnalyzer>>>,
}

impl Expression {
    pub fn new<N: Into<String>>(name: N, size: u32) -> Self {
        let egraph = Arc::new(Mutex::new(EGraph::new(OpAnalyzer)));
        let id = egraph.lock().unwrap().add(Op::Argument(ArgumentInfo {
            size,
            name: name.into(),
        }));
        Self { id, egraph }
    }

    pub fn build(&self) -> RecExpr<Op> {
        let mut runner = Runner::default()
            .with_egraph(self.egraph.lock().unwrap().clone())
            .with_time_limit(std::time::Duration::from_secs(3600))
            .with_node_limit(100_000)
            .with_iter_limit(50);
        runner.roots.push(self.id);

        runner = runner.run(&make_rules());

        crate::extract::extract(&runner.egraph, runner.roots[0], AstSize)
    }

    pub fn argument<N: Into<String>>(&self, name: N, size: u32) -> Self {
        let id = self.egraph.lock().unwrap().add(Op::Argument(ArgumentInfo {
            size,
            name: name.into(),
        }));
        Self {
            id,
            egraph: self.egraph.clone(),
        }
    }

    pub fn constant<T: Into<BigUint>>(&self, value: T) -> Self {
        let id = self.egraph.lock().unwrap().add(Op::Constant(value.into()));
        Expression {
            id,
            egraph: self.egraph.clone(),
        }
    }

    pub fn ternary<T: IntoId>(&self, then: T, or: T) -> Expression {
        let then_id = then.id(&self.egraph);
        let or_id = or.id(&self.egraph);

        let id = self
            .egraph
            .lock()
            .unwrap()
            .add(Op::Ternary([self.id, then_id, or_id]));
        Expression {
            id,
            egraph: self.egraph.clone(),
        }
    }
}

pub trait IntoId {
    fn id(&self, egraph: &Arc<Mutex<EGraph<Op, OpAnalyzer>>>) -> Id;
}

impl IntoId for Expression {
    fn id(&self, egraph: &Arc<Mutex<EGraph<Op, OpAnalyzer>>>) -> Id {
        assert!(Arc::ptr_eq(&self.egraph, egraph));
        self.id
    }
}

impl<T: Into<BigUint> + Clone> IntoId for T {
    fn id(&self, egraph: &Arc<Mutex<EGraph<Op, OpAnalyzer>>>) -> Id {
        egraph
            .lock()
            .unwrap()
            .add(Op::Constant((*self).clone().into()))
    }
}

impl std::ops::Not for Expression {
    type Output = Self;

    fn not(self) -> Self::Output {
        let id = self.egraph.lock().unwrap().add(Op::Not(self.id));
        Expression {
            id,
            egraph: self.egraph,
        }
    }
}

macro_rules! impl_op {
    ($std_op:ident, $op_func:ident, $lang_op:ident) => {
        impl<T: IntoId> std::ops::$std_op<T> for Expression {
            type Output = Self;

            fn $op_func(self, rhs: T) -> Self::Output {
                let rhs_id = rhs.id(&self.egraph);
                let id = self
                    .egraph
                    .lock()
                    .unwrap()
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

#[cfg(feature = "python")]
#[pyclass]
#[derive(Clone)]
pub struct Expr(Expression);

#[cfg(feature = "python")]
#[derive(FromPyObject)]
enum RhsTypes {
    Const(u64),
    Expr(Expr),
}

#[cfg(feature = "python")]
impl RhsTypes {
    fn into_expression(self, expr: &Expr) -> Expression {
        match self {
            RhsTypes::Const(c) => expr.constant(c).0,
            RhsTypes::Expr(e) => e.0,
        }
    }
}

#[cfg(feature = "python")]
#[pymethods]
impl Expr {
    #[new]
    fn new(name: String, size: u32) -> Self {
        Self(Expression::new(name, size))
    }

    fn argument(&self, name: String, size: u32) -> Self {
        Self(self.0.argument(name, size))
    }

    fn constant(&self, value: u64) -> Self {
        Self(self.0.constant(value))
    }

    fn ternary(&self, then: RhsTypes, or: RhsTypes) -> Self {
        Self(
            self.0
                .ternary(then.into_expression(self), or.into_expression(self)),
        )
    }

    fn compile(&self) -> Circuit {
        let op = self.0.build();
        let logic = crate::logic::Logificator::new(op).build_logic();
        crate::compiler::Compiler::new(&logic).compile()
    }

    fn __invert__(&self) -> Self {
        Self(!self.0.clone())
    }

    fn __xor__(&self, rhs: RhsTypes) -> Self {
        Self(self.0.clone() ^ rhs.into_expression(self))
    }

    fn __or__(&self, rhs: RhsTypes) -> Self {
        Self(self.0.clone() | rhs.into_expression(self))
    }

    fn __and__(&self, rhs: RhsTypes) -> Self {
        Self(self.0.clone() & rhs.into_expression(self))
    }

    fn __rshift__(&self, rhs: u32) -> Self {
        Self(self.0.clone() >> rhs)
    }

    fn __lshift__(&self, rhs: u32) -> Self {
        Self(self.0.clone() << rhs)
    }

    fn __add__(&self, rhs: RhsTypes) -> Self {
        Self(self.0.clone() + rhs.into_expression(self))
    }

    fn __sub__(&self, rhs: RhsTypes) -> Self {
        Self(self.0.clone() - rhs.into_expression(self))
    }

    fn __mul__(&self, rhs: RhsTypes) -> Self {
        Self(self.0.clone() * rhs.into_expression(self))
    }

    fn __divmod__(&self, rhs: RhsTypes) -> Self {
        Self(self.0.clone() / rhs.into_expression(self))
    }
}
