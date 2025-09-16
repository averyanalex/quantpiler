use std::{fmt::Display, str::FromStr};

use egg::{Id, Language, define_language};

/// Represents qubit in argument
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ArgInfo {
    /// Name of argument with qubit
    pub name: String,
    /// Index of qubit in argument
    pub index: usize,
}

impl Display for ArgInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}[{}]", self.name, self.index)
    }
}

impl FromStr for ArgInfo {
    type Err = ();

    fn from_str(_s: &str) -> Result<Self, Self::Err> {
        Err(())
    }
}

define_language! {
    /// Qubit language
    pub enum QubitLanguage {
        // Xor logic gate
        "^" = Xor([Id; 2]),
        // And logic gate
        "&" = And([Id; 2]),
        // Not logic gate
        "!" = Not(Id),
        // Merge gates into register, useful for return op
        "r" = Register(Box<[Id]>),
        // Constant value
        Const(bool),
        // Argument qubit
        Arg(ArgInfo),
    }
}
