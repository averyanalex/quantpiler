use std::fmt::Display;

use rustc_hash::{FxHashMap, FxHashSet};

#[derive(Debug, Clone)]
pub enum QGate {
    X(u32),
    CX {
        control: u32,
        target: u32,
    },
    Toffoli {
        control_0: u32,
        control_1: u32,
        target: u32,
    },
    MultiCX {
        controls: FxHashSet<u32>,
        target: u32,
    },
    // Payload,
}

impl QGate {
    pub fn format_qasm(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        map: &FxHashMap<u32, QubitDesc>,
    ) -> std::fmt::Result {
        match self {
            QGate::X(arg) => f.write_fmt(format_args!("x {}", map[arg])),
            QGate::CX { control, target } => {
                f.write_fmt(format_args!("cx {}, {}", map[control], map[target]))
            }
            QGate::Toffoli {
                control_0,
                control_1,
                target,
            } => f.write_fmt(format_args!(
                "ccx {}, {}, {}",
                map[control_0], map[control_1], map[target]
            )),
            QGate::MultiCX { controls, target } => f.write_fmt(format_args!("mcx CRINGE FIXME")),
        }
    }
}

#[derive(Debug)]
pub struct Qubit(u32);

impl Qubit {
    pub fn new(id: u32) -> Self {
        Self(id)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum QubitRegister {
    Ancillary,
    Result,
    Argument(String),
}

impl Display for QubitRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            QubitRegister::Ancillary => f.write_str("ancilla"),
            QubitRegister::Result => f.write_str("result"),
            QubitRegister::Argument(arg) => f.write_fmt(format_args!("{arg}")),
        }
    }
}

#[derive(Debug, Clone)]
pub struct QubitDesc {
    pub reg: QubitRegister,
    pub index: u32,
}

impl Display for QubitDesc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}[{}]", self.reg, self.index))
    }
}

#[derive(Debug)]
pub struct Circuit {
    pub next_id: u32,
    free_ancillas: Vec<Qubit>,
    pub gates: Vec<QGate>,
    qubits_map: FxHashMap<u32, QubitDesc>,
    arguments: Vec<(String, u32)>,
}

impl Circuit {
    pub fn new(args: Vec<(String, u32)>) -> Self {
        Self {
            next_id: 0,
            free_ancillas: Vec::new(),
            gates: Vec::new(),
            qubits_map: FxHashMap::default(),
            arguments: args,
        }
    }

    pub fn get_free_qubit(&mut self) -> Qubit {
        match self.free_ancillas.pop() {
            Some(q) => q,
            None => {
                let q = Qubit::new(self.next_id);
                self.next_id = self.next_id + 1;
                q
            }
        }
    }

    pub fn set_qubit_desc(&mut self, qubit: &Qubit, desc: QubitDesc) {
        self.qubits_map.insert(qubit.0, desc);
    }

    pub fn x(&mut self, target: Qubit) -> Qubit {
        self.gates.push(QGate::X(target.0.clone()));
        target
    }

    pub fn cx(&mut self, control: &Qubit, target: Qubit) -> Qubit {
        self.gates.push(QGate::CX {
            control: control.0.clone(),
            target: target.0.clone(),
        });
        target
    }

    pub fn mcx(&mut self, controls: Vec<&Qubit>, target: Qubit) -> Qubit {
        if controls.len() == 2 {
            self.gates.push(QGate::Toffoli {
                control_0: controls[0].0.clone(),
                control_1: controls[1].0.clone(),
                target: target.0.clone(),
            })
        } else {
            println!("NOOOOO, {} controls", controls.len());
            self.gates.push(QGate::MultiCX {
                controls: controls.into_iter().map(|q| q.0.clone()).collect(),
                target: target.0.clone(),
            });
        }
        target
    }
}

impl Display for Circuit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut map = self.qubits_map.clone();
        let mut anc_index = 0;
        for id in 0..self.next_id {
            map.entry(id).or_insert(QubitDesc {
                reg: QubitRegister::Ancillary,
                index: anc_index,
            });
            anc_index += 1;
        }

        f.write_str("OPENQASM 3.0;\n")?;
        f.write_str("include \"stdgates.inc\";\n\n")?;

        f.write_str("def app")?;

        f.write_fmt(format_args!(" qubit[{anc_index}] ancilla"))?;
        f.write_fmt(format_args!(
            ", qubit[{}] result",
            map.iter()
                .filter(|(k, v)| v.reg == QubitRegister::Result)
                .count()
        ))?;
        for (name, len) in &self.arguments {
            f.write_fmt(format_args!(", qubit[{}] {}", len, name))?;
        }
        f.write_str(" {\n")?;

        for gate in &self.gates {
            f.write_str("  ")?;
            gate.format_qasm(f, &map)?;
            f.write_str(";\n")?;
        }

        // f.write_str("  reset ancillas;\n")?;

        f.write_str("}\n")?;
        Ok(())
    }
}
