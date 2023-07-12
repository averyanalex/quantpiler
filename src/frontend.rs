#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(dead_code)]
pub enum Ast {
    Function {
        name: String,
        arguments: Vec<(String, u32)>,
        instructions: Vec<Self>,
    },
    Variable(String),
    Constant(u32),
    Assignment {
        variable: String,
        value: Box<Self>,
    },
    Ternary {
        condition: Box<Self>,
        then: Box<Self>,
        or: Box<Self>,
    },
    Index {
        index: Box<Self>,
        target: Box<Self>,
    },
    IndexRange {
        target: Box<Self>,
        from: Box<Self>,
        to: Box<Self>,
    },
    StaticForLoop {
        variable: String,
        values: Vec<Self>,
        instructions: Vec<Self>,
    },
    Xor(Vec<Self>),
    Or(Vec<Self>),
    And(Vec<Self>),
    Not(Box<Self>),
    Return(Box<Self>),
    RShift {
        target: Box<Self>,
        distance: Box<Self>,
    },
}

impl Ast {
    pub fn function(name: &str, arguments: Vec<(&str, u32)>, instructions: Vec<Self>) -> Self {
        Self::Function {
            name: name.into(),
            arguments: arguments
                .into_iter()
                .map(|(name, size)| (name.into(), size))
                .collect(),
            instructions,
        }
    }

    pub fn assign(variable: &str, value: Self) -> Self {
        Self::Assignment {
            variable: variable.into(),
            value: Box::new(value),
        }
    }

    pub fn constant(value: u32) -> Self {
        Self::Constant(value)
    }

    pub fn and(values: Vec<Self>) -> Self {
        Self::And(values)
    }

    pub fn xor(values: Vec<Self>) -> Self {
        Self::Xor(values)
    }

    pub fn variable(name: &str) -> Self {
        Self::Variable(name.into())
    }

    pub fn static_for_loop(variable: &str, values: Vec<Self>, instructions: Vec<Self>) -> Self {
        Self::StaticForLoop {
            variable: variable.into(),
            values,
            instructions,
        }
    }

    pub fn ternary(condition: Self, then: Self, or: Self) -> Self {
        Self::Ternary {
            condition: condition.into(),
            then: then.into(),
            or: or.into(),
        }
    }

    pub fn rshift(target: Self, distance: Self) -> Self {
        Self::RShift {
            target: target.into(),
            distance: distance.into(),
        }
    }
}

pub fn parse() -> Ast {
    Ast::function(
        "crc32",
        vec![("a", 8), ("b", 8), ("c", 8), ("d", 8)],
        vec![
            Ast::assign("poly", Ast::constant(0xEDB88320)),
            Ast::assign("value", Ast::constant(0xFFFFFFFF)),
            Ast::static_for_loop(
                "st",
                ["a", "b", "c", "d"]
                    .into_iter()
                    .map(Ast::variable)
                    .collect(),
                vec![
                    Ast::assign(
                        "ch",
                        Ast::and(vec![
                            Ast::xor(vec![Ast::variable("st"), Ast::variable("value")]),
                            Ast::constant(0xFF),
                        ]),
                    ),
                    Ast::assign("table", Ast::constant(0)),
                    Ast::static_for_loop(
                        "bit",
                        (0..8).map(Ast::constant).collect(),
                        vec![
                            Ast::assign(
                                "table",
                                Ast::ternary(
                                    Ast::and(vec![
                                        Ast::xor(vec![Ast::variable("ch"), Ast::variable("table")]),
                                        Ast::constant(1),
                                    ]),
                                    Ast::xor(vec![
                                        Ast::rshift(Ast::variable("table"), Ast::constant(1)),
                                        Ast::variable("poly"),
                                    ]),
                                    Ast::rshift(Ast::variable("table"), Ast::constant(1)),
                                ),
                            ),
                            Ast::assign("ch", Ast::rshift(Ast::variable("ch"), Ast::constant(1))),
                        ],
                    ),
                    Ast::assign(
                        "value",
                        Ast::xor(vec![
                            Ast::variable("table"),
                            Ast::rshift(Ast::variable("value"), Ast::constant(8)),
                        ]),
                    ),
                ],
            ),
            Ast::Return(Box::new(Ast::variable("value"))),
        ],
    )
}
