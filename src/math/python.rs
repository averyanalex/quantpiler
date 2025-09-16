use std::sync::Arc;

use ouroboros::self_referencing;
use pyo3::prelude::*;

use super::expression::Expression as RustExpression;
use super::expression::ExpressionScope as RustExpressionScope;

#[pyclass]
pub struct ExpressionScope {
    scope: Arc<RustExpressionScope>,
}

#[pyclass]
#[self_referencing]
pub struct Expression {
    scope: Arc<RustExpressionScope>,
    #[borrows(scope)]
    #[covariant]
    expression: RustExpression<'this>,
}

impl Expression {
    fn binary_op<'a, F: FnOnce(RustExpression<'a>, RustExpression<'a>) -> RustExpression<'a>>(
        &'a self,
        other: &'a Self,
        op: F,
    ) -> Self {
        assert!(
            Arc::ptr_eq(self.borrow_scope(), other.borrow_scope()),
            "Cannot use expressions with different scopes"
        );

        ExpressionBuilder {
            scope: self.borrow_scope().clone(),
            expression_builder: |scope: &Arc<RustExpressionScope>| {
                op(*self.borrow_expression(), *other.borrow_expression()).ensure_scope(scope)
            },
        }
        .build()
    }
}

#[pymethods]
impl Expression {
    fn __xor__(&self, other: &Self) -> Self {
        self.binary_op(other, |a, b| a ^ b)
    }
}
