#![warn(clippy::pedantic, clippy::nursery)]
#![allow(
    clippy::wildcard_imports,
    clippy::enum_glob_use,
    clippy::too_many_lines,
    clippy::must_use_candidate,
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::trivially_copy_pass_by_ref,
    clippy::redundant_closure_for_method_calls,
    clippy::cast_sign_loss,
    clippy::cast_possible_truncation,
    clippy::implicit_hasher,
    clippy::tuple_array_conversions
)]

/*!
`quantpiler` (**quant**um com**piler**) is a compiler of classical algorithms into oracles for quantum computing.
*/

pub mod circuit;
pub mod compiler;
#[cfg(test)]
mod executor;
pub mod expression;
mod extract;
pub mod logic;
pub mod op;
#[cfg(test)]
mod verify;

pub use expression::Expression;

#[cfg(test)]
mod tests;

#[cfg(feature = "python")]
use pyo3::prelude::*;

#[cfg(feature = "python")]
#[pymodule]
pub fn quantpiler(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<expression::Expr>()?;
    m.add_class::<circuit::Circuit>()?;
    m.add_function(wrap_pyfunction!(expression::argument, m)?)?;
    m.add_function(wrap_pyfunction!(expression::constant, m)?)?;
    Ok(())
}
