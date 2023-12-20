#[cfg(feature = "python")]
use std::sync::LazyLock;
use std::sync::{Arc, Mutex};

use egg::*;
use num::BigUint;
#[cfg(feature = "python")]
use pyo3::prelude::*;

#[cfg(feature = "python")]
use crate::circuit::Circuit;
use crate::op::{make_rules, ArgumentInfo, Op, OpAnalyzer, OpCost};

#[derive(Clone)]
pub struct Expression {
    id: Id,
    egraph: Arc<Mutex<EGraph<Op, OpAnalyzer>>>,
}

impl Expression {
    pub fn new_argument<N: Into<String>>(name: N, size: u32) -> Self {
        let egraph = Arc::new(Mutex::new(EGraph::new(OpAnalyzer)));
        let id = egraph.lock().unwrap().add(Op::Argument(ArgumentInfo {
            size,
            name: name.into(),
        }));
        Self { id, egraph }
    }

    pub fn new_constant<T: Into<BigUint>>(value: T) -> Self {
        let egraph = Arc::new(Mutex::new(EGraph::new(OpAnalyzer)));
        let id = egraph.lock().unwrap().add(Op::Constant(value.into()));
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

        crate::extract::extract(&runner.egraph, runner.roots[0], OpCost)
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

    pub fn ternary<T1: IntoId, T2: IntoId>(&self, then: T1, or: T2) -> Expression {
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
impl_op! {Rem, rem, Rem}

#[cfg(feature = "python")]
static ZERO_EXPR: LazyLock<Expression> = LazyLock::new(|| Expression::new_constant(0u32));

#[cfg(feature = "python")]
#[pyfunction]
pub fn argument(name: String, size: u32) -> Expr {
    Expr(ZERO_EXPR.argument(name, size))
}

#[cfg(feature = "python")]
#[pyfunction]
pub fn constant(value: u128) -> Expr {
    Expr(ZERO_EXPR.constant(value))
}

#[cfg(feature = "python")]
#[pyclass]
#[derive(Clone)]
pub struct Expr(Expression);

#[cfg(feature = "python")]
#[derive(FromPyObject)]
enum RhsTypes {
    Const(u128),
    Expr(Expr),
}

#[cfg(feature = "python")]
impl RhsTypes {
    fn expr(self) -> Expression {
        match self {
            RhsTypes::Const(c) => ZERO_EXPR.constant(c),
            RhsTypes::Expr(e) => e.0,
        }
    }
}

#[cfg(feature = "python")]
#[pymethods]
impl Expr {
    fn ternary(&self, then: RhsTypes, or: RhsTypes) -> Self {
        Self(self.0.ternary(then.expr(), or.expr()))
    }

    fn compile(&self) -> Circuit {
        crate::compile(&self.0)
    }

    fn __str__(&self) -> String {
        let op = self.0.build();
        op.to_string()
    }

    fn __repr__(&self) -> String {
        self.__str__()
    }

    fn __len__(&self) -> usize {
        let op = self.0.build();
        let logic = crate::logic::Logificator::new(op).build_logic();
        logic[Id::from(logic.as_ref().len() - 1)].children().len()
    }

    // fn __repr__(&self) -> String {
    //     let op = self.0.build();
    //     format!("{:?}", op)
    // }

    fn __invert__(&self) -> Self {
        Self(!self.0.clone())
    }

    fn __xor__(&self, rhs: RhsTypes) -> Self {
        Self(self.0.clone() ^ rhs.expr())
    }

    fn __rxor__(&self, rhs: RhsTypes) -> Self {
        self.__xor__(rhs)
    }

    fn __ixor__(&mut self, rhs: RhsTypes) {
        *self = self.__xor__(rhs);
    }

    fn __or__(&self, rhs: RhsTypes) -> Self {
        Self(self.0.clone() | rhs.expr())
    }

    fn __ror__(&self, rhs: RhsTypes) -> Self {
        self.__or__(rhs)
    }

    fn __ior__(&mut self, rhs: RhsTypes) {
        *self = self.__or__(rhs);
    }

    fn __and__(&self, rhs: RhsTypes) -> Self {
        Self(self.0.clone() & rhs.expr())
    }

    fn __rand__(&self, rhs: RhsTypes) -> Self {
        self.__and__(rhs)
    }

    fn __iand__(&mut self, rhs: RhsTypes) {
        *self = self.__and__(rhs);
    }

    fn __rshift__(&self, rhs: u128) -> Self {
        Self(self.0.clone() >> rhs)
    }

    fn __rrshift__(&self, rhs: u128) -> Self {
        self.__rshift__(rhs)
    }

    fn __irshift__(&mut self, rhs: u128) {
        *self = self.__rshift__(rhs);
    }

    fn __lshift__(&self, rhs: u128) -> Self {
        Self(self.0.clone() << rhs)
    }

    fn __rlshift__(&self, rhs: u128) -> Self {
        self.__lshift__(rhs)
    }

    fn __ilshift__(&mut self, rhs: u128) {
        *self = self.__lshift__(rhs);
    }

    fn __add__(&self, rhs: RhsTypes) -> Self {
        Self(self.0.clone() + rhs.expr())
    }

    fn __radd__(&self, rhs: RhsTypes) -> Self {
        self.__add__(rhs)
    }

    fn __iadd__(&mut self, rhs: RhsTypes) {
        *self = self.__add__(rhs);
    }

    fn __sub__(&self, rhs: RhsTypes) -> Self {
        Self(self.0.clone() - rhs.expr())
    }

    fn __rsub__(&self, rhs: RhsTypes) -> Self {
        self.__sub__(rhs)
    }

    fn __isub__(&mut self, rhs: RhsTypes) {
        *self = self.__sub__(rhs);
    }

    fn __mul__(&self, rhs: RhsTypes) -> Self {
        Self(self.0.clone() * rhs.expr())
    }

    fn __rmul__(&self, rhs: RhsTypes) -> Self {
        self.__mul__(rhs)
    }

    fn __imul__(&mut self, rhs: RhsTypes) {
        *self = self.__mul__(rhs);
    }

    fn __floordiv__(&self, rhs: RhsTypes) -> Self {
        Self(self.0.clone() / rhs.expr())
    }

    fn __rfloordiv__(&self, rhs: RhsTypes) -> Self {
        self.__floordiv__(rhs)
    }

    fn __ifloordiv__(&mut self, rhs: RhsTypes) {
        *self = self.__floordiv__(rhs);
    }

    fn __mod__(&self, rhs: RhsTypes) -> Self {
        Self(self.0.clone() % rhs.expr())
    }

    fn __rmod__(&self, rhs: RhsTypes) -> Self {
        self.__mod__(rhs)
    }

    fn __imod__(&mut self, rhs: RhsTypes) {
        *self = self.__mod__(rhs);
    }
}
