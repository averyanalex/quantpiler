#![feature(let_chains)]

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
    Ok(())
}
