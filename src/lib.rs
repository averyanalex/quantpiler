#![feature(let_chains)]
#![feature(lazy_cell)]

pub mod circuit;
pub mod compiler;
pub mod executor;
pub mod expression;
pub mod extract;
pub mod logic;
pub mod op;

#[cfg(test)]
mod tests;

#[cfg(feature = "python")]
use pyo3::prelude::*;

#[cfg(feature = "python")]
#[pymodule]
fn quantpiler(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<expression::Expr>()?;
    m.add_class::<circuit::Circuit>()?;
    m.add_wrapped(wrap_pyfunction!(expression::argument))?;
    m.add_wrapped(wrap_pyfunction!(expression::constant))?;
    Ok(())
}
