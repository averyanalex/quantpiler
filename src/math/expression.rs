use std::collections::HashMap;
use std::sync::Mutex;

use egg::*;
use num::{BigUint, Zero};

use super::language::ArgumentInfo;
use super::language::{MathAnalyzer, MathLanguage};

#[derive(Default)]
pub struct ExpressionScope {
    egraph: Mutex<EGraph<MathLanguage, MathAnalyzer>>,
    arguments: Mutex<HashMap<String, (u32, Id)>>,
}

impl ExpressionScope {
    /// Create new expression scope.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create new argument.
    ///
    /// You can't use arguments with the same `name` but diffirent `size` in the same [ExpressionScope].
    pub fn argument<N: Into<String>>(&self, name: N, size: u32) -> Expression<'_> {
        let name = name.into();
        let mut arguments = self.arguments.lock().unwrap();

        // Check if argument already exists with different size
        if let Some((existing_size, existing_id)) = arguments.get(&name) {
            assert_eq!(
                *existing_size, size,
                "Argument '{}' already exists with size {} but requested size {}",
                name, existing_size, size
            );
            return Expression {
                length: size,
                id: *existing_id,
                scope: self,
            };
        }

        // Create new argument
        let id = self.egraph.lock().unwrap().add(MathLanguage::Argument(ArgumentInfo {
            length: size,
            name: name.clone(),
        }));

        arguments.insert(name, (size, id));

        Expression {
            length: size,
            id,
            scope: self,
        }
    }

    /// Create new constant.
    ///
    /// You can use any [`Into<BigUint>`] value.
    pub fn constant<T: Into<BigUint>>(&self, value: T) -> Expression<'_> {
        let value = value.into();
        let length = if value.is_zero() {
            1
        } else {
            value.bits() as u32
        };

        Expression {
            length,
            id: self.egraph.lock().unwrap().add(MathLanguage::Constant(value)),
            scope: self,
        }
    }
}

#[derive(Clone, Copy)]
pub struct Expression<'a> {
    length: u32,
    id: Id,
    scope: &'a ExpressionScope,
}

impl<'a> Expression<'a> {
    /// Get scope of the expression.
    pub fn scope(&self) -> &'a ExpressionScope {
        self.scope
    }

    /// Ensure that expression is in the same scope.
    pub fn ensure_scope<'b>(self, scope: &'b ExpressionScope) -> Expression<'b> {
        assert!(
            std::ptr::eq(self.scope, scope),
            "Cannot change scope, only lifetime"
        );

        Expression {
            length: self.length,
            id: self.id,
            scope,
        }
    }
}

trait IntoExpression {
    fn into_expression<'a>(self, scope: &'a ExpressionScope) -> Expression<'a>;
}

impl IntoExpression for Expression<'_> {
    fn into_expression<'a>(self, scope: &'a ExpressionScope) -> Expression<'a> {
        self.ensure_scope(scope)
    }
}

impl<T: Into<BigUint>> IntoExpression for T {
    fn into_expression<'a>(self, scope: &'a ExpressionScope) -> Expression<'a> {
        scope.constant(self)
    }
}

macro_rules! impl_op {
    ($std_op:ident, $op_func:ident, $lang_op:ident) => {
        impl<T: IntoExpression> std::ops::$std_op<T> for Expression<'_> {
            type Output = Self;

            fn $op_func(self, rhs: T) -> Self::Output {
                let rhs_expr = rhs.into_expression(self.scope);
                Self {
                    length: self.length.max(rhs_expr.length),
                    id: self
                        .scope
                        .egraph
                        .lock()
                        .unwrap()
                        .add(MathLanguage::$lang_op([self.id, rhs_expr.id])),
                    scope: self.scope,
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

// impl ExpressionScope {
//     pub fn new() -> Self {
//         Self {
//             egraph: Arc::new(Mutex::new(EGraph::new(Analyzer::default()))),
//         }
//     }
// }

// #[pymethods]
// impl ExpressionScope {
//     /// Create new expression scope.
//     #[staticmethod]
//     pub fn new() -> Self {
//         Self {
//             egraph: Arc::new(Mutex::new(EGraph::new(Analyzer::default()))),
//         }
//     }

//     /// Create new constant.
//     ///
//     /// You can use any [`Into<BigUint>`] value.
//     #[must_use]
//     pub fn constant<T: Into<BigUint>>(&self, value: T) -> Expression {
//         Expression {
//             length: 0,
//             id: self.egraph.lock().unwrap().add(Op::Constant(value.into())),
//             egraph: self.egraph.clone(),
//         }
//     }
// }

// /// Combination of operations on arguments and constants
// #[derive(Clone)]
// pub struct Expression {
//     length: u32,
//     id: Id,
//     egraph: Arc<Mutex<EGraph<Op, Analyzer>>>,
// }

// impl Expression {
//     /// Build ternary operation. [Expression] will be `then` if `self` is \[true\] and `or` otherwise.
//     ///
//     /// This can emulate something like
//     /// ```
//     /// if expr { then } else { or }
//     /// ```
//     #[must_use]
//     pub fn ternary<T1: IntoId, T2: IntoId>(self, then: T1, or: T2) -> Self {
//         let ids = [self.id(), then.id(), or.id()];
//         Self(OP_EGRAPH.lock().unwrap().add(Op::Ternary(ids)))
//     }

//     #[must_use]
//     pub fn build(self) -> RecExpr<Op> {
//         let egraph = OP_EGRAPH.lock().unwrap().clone();
//         let cleaned_expr = crate::extract::extract(&egraph, self.id(), Cost);

//         let mut runner = Runner::default()
//             .with_expr(&cleaned_expr)
//             .with_time_limit(std::time::Duration::from_secs(3600))
//             .with_node_limit(50_000)
//             .with_iter_limit(100);
//         runner = runner.run(&make_rules());
//         crate::extract::extract(&runner.egraph, runner.roots[0], Cost)
//     }

//     /// Compile expression into quantum circuit
//     pub fn compile(self) -> crate::circuit::Circuit {
//         let op = self.build();
//         #[allow(clippy::redundant_clone)]
//         let logic = crate::logic::Logificator::new(op.clone()).build_logic();
//         #[allow(clippy::let_and_return)]
//         let circuit = crate::compiler::Compiler::new(&logic).compile();

//         #[cfg(test)]
//         crate::verify::verify(&op, &logic, &circuit);

//         circuit
//     }

//     #[must_use]
//     pub fn eq(self, other: Self) -> Self {
//         Self(
//             OP_EGRAPH
//                 .lock()
//                 .unwrap()
//                 .add(Op::Eq([self.id(), other.id()])),
//         )
//     }

//     #[must_use]
//     pub fn gt(self, other: Self) -> Self {
//         Self(
//             OP_EGRAPH
//                 .lock()
//                 .unwrap()
//                 .add(Op::Gt([self.id(), other.id()])),
//         )
//     }

//     #[must_use]
//     pub fn lt(self, other: Self) -> Self {
//         Self(
//             OP_EGRAPH
//                 .lock()
//                 .unwrap()
//                 .add(Op::Lt([self.id(), other.id()])),
//         )
//     }

//     #[must_use]
//     pub fn ne(self, other: Self) -> Self {
//         Self(
//             OP_EGRAPH
//                 .lock()
//                 .unwrap()
//                 .add(Op::Ne([self.id(), other.id()])),
//         )
//     }

//     #[must_use]
//     pub fn le(self, other: Self) -> Self {
//         Self(
//             OP_EGRAPH
//                 .lock()
//                 .unwrap()
//                 .add(Op::Le([self.id(), other.id()])),
//         )
//     }

//     #[must_use]
//     pub fn ge(self, other: Self) -> Self {
//         Self(
//             OP_EGRAPH
//                 .lock()
//                 .unwrap()
//                 .add(Op::Ge([self.id(), other.id()])),
//         )
//     }

//     #[must_use]
//     pub fn add_rem(self, rhs: Self, modulus: Self) -> Self {
//         Self(
//             OP_EGRAPH
//                 .lock()
//                 .unwrap()
//                 .add(Op::AddRem([self.id(), rhs.id(), modulus.id()])),
//         )
//     }

//     #[must_use]
//     pub fn mul_rem(self, rhs: Self, modulus: Self) -> Self {
//         Self(
//             OP_EGRAPH
//                 .lock()
//                 .unwrap()
//                 .add(Op::MulRem([self.id(), rhs.id(), modulus.id()])),
//         )
//     }

//     #[must_use]
//     pub fn pow(self, rhs: Self) -> Self {
//         Self(
//             OP_EGRAPH
//                 .lock()
//                 .unwrap()
//                 .add(Op::Pow([self.id(), rhs.id()])),
//         )
//     }

//     #[must_use]
//     pub fn pow_rem(self, rhs: Self, modulus: Self) -> Self {
//         Self(
//             OP_EGRAPH
//                 .lock()
//                 .unwrap()
//                 .add(Op::PowRem([self.id(), rhs.id(), modulus.id()])),
//         )
//     }
// }

// pub trait IntoId {
//     fn id(self) -> Id;
// }

// impl IntoId for Expression {
//     fn id(self) -> Id {
//         self.0
//     }
// }

// impl<T: Into<BigUint>> IntoId for T {
//     fn id(self) -> Id {
//         OP_EGRAPH.lock().unwrap().add(Op::Constant(self.into()))
//     }
// }

// impl std::ops::Not for Expression {
//     type Output = Self;

//     fn not(self) -> Self::Output {
//         Self(OP_EGRAPH.lock().unwrap().add(Op::Not(self.id())))
//     }
// }

// macro_rules! impl_op {
//     ($std_op:ident, $op_func:ident, $lang_op:ident) => {
//         impl<T: IntoId> std::ops::$std_op<T> for Expression {
//             type Output = Self;

//             fn $op_func(self, rhs: T) -> Self::Output {
//                 let ids = [self.id(), rhs.id()];
//                 Self(OP_EGRAPH.lock().unwrap().add(Op::$lang_op(ids)))
//             }
//         }
//     };
// }

// impl_op! {BitXor, bitxor, Xor}
// impl_op! {BitOr, bitor, Or}
// impl_op! {BitAnd, bitand, And}

// impl_op! {Shr, shr, Shr}
// impl_op! {Shl, shl, Shl}

// impl_op! {Add, add, Add}
// impl_op! {Sub, sub, Sub}

// impl_op! {Mul, mul, Mul}
// impl_op! {Div, div, Div}
// impl_op! {Rem, rem, Rem}

// #[cfg(feature = "python")]
// #[pyfunction]
// pub fn argument(name: String, size: u32) -> Expr {
//     Expr(Expression::argument(name, size as _))
// }

// #[cfg(feature = "python")]
// #[pyfunction]
// pub fn constant(value: u128) -> Expr {
//     Expr(Expression::constant(value))
// }

// #[cfg(feature = "python")]
// #[pyclass]
// #[derive(Clone, Copy)]
// pub struct Expr(Expression);

// #[cfg(feature = "python")]
// #[derive(FromPyObject)]
// enum RhsTypes {
//     Const(u128),
//     Expr(Expr),
// }

// #[cfg(feature = "python")]
// impl RhsTypes {
//     fn expr(self) -> Expression {
//         match self {
//             Self::Const(c) => Expression::constant(c),
//             Self::Expr(e) => e.0,
//         }
//     }
// }

// #[cfg(feature = "python")]
// #[pymethods]
// impl Expr {
//     fn ternary(&self, then: RhsTypes, or: RhsTypes) -> Self {
//         Self(self.0.ternary(then.expr(), or.expr()))
//     }

//     fn compile(&self) -> Circuit {
//         self.0.compile()
//     }

//     fn __str__(&self) -> String {
//         let op = self.0.build();
//         op.to_string()
//     }

//     fn __repr__(&self) -> String {
//         self.__str__()
//     }

//     fn __len__(&self) -> usize {
//         let op = self.0.build();
//         let logic = crate::logic::Logificator::new(op).build_logic();
//         logic[Id::from(logic.as_ref().len() - 1)].children().len()
//     }

//     // fn __repr__(&self) -> String {
//     //     let op = self.0.build();
//     //     format!("{:?}", op)
//     // }

//     fn __invert__(&self) -> Self {
//         Self(!self.0)
//     }

//     fn __xor__(&self, rhs: RhsTypes) -> Self {
//         Self(self.0 ^ rhs.expr())
//     }

//     fn __rxor__(&self, rhs: RhsTypes) -> Self {
//         self.__xor__(rhs)
//     }

//     fn __ixor__(&mut self, rhs: RhsTypes) {
//         *self = self.__xor__(rhs);
//     }

//     fn __or__(&self, rhs: RhsTypes) -> Self {
//         Self(self.0 | rhs.expr())
//     }

//     fn __ror__(&self, rhs: RhsTypes) -> Self {
//         self.__or__(rhs)
//     }

//     fn __ior__(&mut self, rhs: RhsTypes) {
//         *self = self.__or__(rhs);
//     }

//     fn __and__(&self, rhs: RhsTypes) -> Self {
//         Self(self.0 & rhs.expr())
//     }

//     fn __rand__(&self, rhs: RhsTypes) -> Self {
//         self.__and__(rhs)
//     }

//     fn __iand__(&mut self, rhs: RhsTypes) {
//         *self = self.__and__(rhs);
//     }

//     fn __rshift__(&self, rhs: u128) -> Self {
//         Self(self.0 >> rhs)
//     }

//     fn __rrshift__(&self, rhs: u128) -> Self {
//         self.__rshift__(rhs)
//     }

//     fn __irshift__(&mut self, rhs: u128) {
//         *self = self.__rshift__(rhs);
//     }

//     fn __lshift__(&self, rhs: u128) -> Self {
//         Self(self.0 << rhs)
//     }

//     fn __rlshift__(&self, rhs: u128) -> Self {
//         self.__lshift__(rhs)
//     }

//     fn __ilshift__(&mut self, rhs: u128) {
//         *self = self.__lshift__(rhs);
//     }

//     fn __add__(&self, rhs: RhsTypes) -> Self {
//         Self(self.0 + rhs.expr())
//     }

//     fn __radd__(&self, rhs: RhsTypes) -> Self {
//         self.__add__(rhs)
//     }

//     fn __iadd__(&mut self, rhs: RhsTypes) {
//         *self = self.__add__(rhs);
//     }

//     fn add_rem(&self, rhs: RhsTypes, modulus: RhsTypes) -> Self {
//         Self(self.0.add_rem(rhs.expr(), modulus.expr()))
//     }

//     fn iadd_rem(&mut self, rhs: RhsTypes, modulus: RhsTypes) {
//         self.0 = self.0.add_rem(rhs.expr(), modulus.expr());
//     }

//     fn __sub__(&self, rhs: RhsTypes) -> Self {
//         Self(self.0 - rhs.expr())
//     }

//     fn __rsub__(&self, rhs: RhsTypes) -> Self {
//         self.__sub__(rhs)
//     }

//     fn __isub__(&mut self, rhs: RhsTypes) {
//         *self = self.__sub__(rhs);
//     }

//     fn __mul__(&self, rhs: RhsTypes) -> Self {
//         Self(self.0 * rhs.expr())
//     }

//     fn __rmul__(&self, rhs: RhsTypes) -> Self {
//         self.__mul__(rhs)
//     }

//     fn __imul__(&mut self, rhs: RhsTypes) {
//         *self = self.__mul__(rhs);
//     }

//     fn mul_rem(&self, rhs: RhsTypes, modulus: RhsTypes) -> Self {
//         Self(self.0.mul_rem(rhs.expr(), modulus.expr()))
//     }

//     fn imul_rem(&mut self, rhs: RhsTypes, modulus: RhsTypes) {
//         self.0 = self.0.mul_rem(rhs.expr(), modulus.expr());
//     }

//     fn __floordiv__(&self, rhs: RhsTypes) -> Self {
//         Self(self.0 / rhs.expr())
//     }

//     fn __rfloordiv__(&self, rhs: RhsTypes) -> Self {
//         self.__floordiv__(rhs)
//     }

//     fn __ifloordiv__(&mut self, rhs: RhsTypes) {
//         *self = self.__floordiv__(rhs);
//     }

//     fn __mod__(&self, rhs: RhsTypes) -> Self {
//         Self(self.0 % rhs.expr())
//     }

//     fn __rmod__(&self, rhs: RhsTypes) -> Self {
//         self.__mod__(rhs)
//     }

//     fn __imod__(&mut self, rhs: RhsTypes) {
//         *self = self.__mod__(rhs);
//     }

//     // fn __pow__(&self, rhs: RhsTypes) -> Self {
//     //     Self(self.0.pow(rhs.expr()))
//     // }

//     fn pow_rem(&self, rhs: RhsTypes, modulus: RhsTypes) -> Self {
//         Self(self.0.pow_rem(rhs.expr(), modulus.expr()))
//     }

//     fn ipow_rem(&mut self, rhs: RhsTypes, modulus: RhsTypes) {
//         self.0 = self.0.pow_rem(rhs.expr(), modulus.expr());
//     }

//     fn __richcmp__(&self, other: RhsTypes, op: CompareOp) -> Self {
//         match op {
//             CompareOp::Lt => Self(self.0.lt(other.expr())),
//             CompareOp::Le => Self(self.0.le(other.expr())),
//             CompareOp::Eq => Self(self.0.eq(other.expr())),
//             CompareOp::Ne => Self(self.0.ne(other.expr())),
//             CompareOp::Gt => Self(self.0.gt(other.expr())),
//             CompareOp::Ge => Self(self.0.ge(other.expr())),
//         }
//     }
// }
