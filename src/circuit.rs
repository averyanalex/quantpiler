use std::fmt::Display;

use rustc_hash::{FxHashMap, FxHashSet};

#[derive(Debug)]
pub struct GateX {
    controls: FxHashSet<(Qubit, bool)>,
    target: Qubit,
}

impl GateX {
    pub fn format_qasm(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        map: &FxHashMap<u32, QubitDesc>,
    ) -> std::fmt::Result {
        Ok(())
        // match self {
        //     QGate::X(arg) => f.write_fmt(format_args!("x {}", map[arg])),
        //     QGate::CX { control, target } => {
        //         f.write_fmt(format_args!("cx {}, {}", map[control], map[target]))
        //     }
        //     QGate::Toffoli {
        //         control_0,
        //         control_1,
        //         target,
        //     } => f.write_fmt(format_args!(
        //         "ccx {}, {}, {}",
        //         map[control_0], map[control_1], map[target]
        //     )),
        //     QGate::MultiCX {
        //         controls: _,
        //         target: _,
        //     } => f.write_fmt(format_args!("mcx CRINGE FIXME")),
        // }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Qubit(pub u32);

impl Qubit {
    pub fn new(id: u32) -> Self {
        Self(id)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
            QubitRegister::Argument(arg) => write!(f, "{arg}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    pub qubits_count: u32,
    // free_ancillas: Vec<Qubit>,
    pub gates: Vec<GateX>,
    qubits_map: FxHashMap<Qubit, FxHashSet<QubitDesc>>,
    arguments: Vec<(String, u32)>,
}

impl Circuit {
    pub fn new(args: Vec<(String, u32)>) -> Self {
        Self {
            qubits_count: 0,
            gates: Vec::new(),
            qubits_map: FxHashMap::default(),
            arguments: args,
        }
    }

    pub fn add_qubit_description(&mut self, qubit: Qubit, description: QubitDesc) {
        self.qubits_map
            .entry(qubit)
            .and_modify(|set| {
                // if description.reg == QubitRegister::Result {
                //     assert!(set
                //         .iter()
                //         .all(|desc| matches!(desc.reg, QubitRegister::Argument(_))));
                // }
                set.insert(description.clone());
            })
            .or_insert_with(|| {
                let mut set = FxHashSet::default();
                set.insert(description);
                set
            });
    }

    pub fn get_ancilla_qubit(&mut self) -> Qubit {
        let q = Qubit::new(self.qubits_count);
        self.qubits_count += 1;
        q
    }

    pub fn mcx(&mut self, controls: FxHashSet<(Qubit, bool)>, target: Qubit) {
        self.gates.push(GateX { controls, target });
    }

    pub fn cx(&mut self, source: Qubit, inversed: bool, target: Qubit) {
        self.gates.push(GateX {
            controls: [(source, inversed)].into_iter().collect(),
            target,
        });
    }

    pub fn x(&mut self, target: Qubit) {
        self.gates.push(GateX {
            controls: FxHashSet::default(),
            target,
        });
    }

    pub fn execute(&self, args: &FxHashMap<String, Vec<bool>>) -> Vec<bool> {
        let mut qubits = FxHashMap::default();

        // let qubit_map = self.fill_qubit_map();
        for (qubit, values) in &self.qubits_map {
            for value in values {
                if let QubitRegister::Argument(arg) = value.reg.clone() {
                    qubits.insert(*qubit, args[&arg][value.index as usize]);
                }
            }
        }

        for gate in &self.gates {
            qubits.insert(
                gate.target,
                qubits.get(&gate.target).unwrap_or(&false)
                    ^ gate.controls.iter().fold(true, |acc, (qubit, inverted)| {
                        acc & (qubits[qubit] ^ inverted)
                    }),
            );
        }

        let mut result = vec![
            false;
            self.qubits_map
                .values()
                .map(|set| set
                    .iter()
                    .map(|desc| if desc.reg == QubitRegister::Result {
                        desc.index + 1
                    } else {
                        0
                    })
                    .max()
                    .unwrap())
                .max()
                .unwrap() as usize
        ];

        for (qubit, values) in &self.qubits_map {
            for value in values {
                if let QubitRegister::Result = value.reg {
                    result[value.index as usize] = *qubits.get(qubit).unwrap_or(&false);
                }
            }
        }

        result
    }

    // fn fill_qubit_map(&self) -> FxHashMap<Qubit, QubitDesc> {
    //     let mut map = self.qubits_map.clone();
    //     let mut anc_index = 0;
    //     for id in 0..self.qubits_count {
    //         map.entry(Qubit(id)).or_insert_with(|| {
    //             let desc = QubitDesc {
    //                 reg: QubitRegister::Ancillary,
    //                 index: anc_index,
    //             };
    //             anc_index += 1;
    //             desc
    //         });
    //     }
    //     map
    // }
}

// impl Display for Circuit {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let mut map = self.qubits_map.clone();
//         let mut anc_index = 0;
//         for id in 0..self.next_id {
//             map.entry(id).or_insert(QubitDesc {
//                 reg: QubitRegister::Ancillary,
//                 index: anc_index,
//             });
//             anc_index += 1;
//         }

//         f.write_str("OPENQASM 3.0;\n")?;
//         f.write_str("include \"stdgates.inc\";\n\n")?;

//         f.write_str("def app")?;

//         f.write_fmt(format_args!(" qubit[{anc_index}] ancilla"))?;
//         f.write_fmt(format_args!(
//             ", qubit[{}] result",
//             map.iter()
//                 .filter(|(_k, v)| v.reg == QubitRegister::Result)
//                 .count()
//         ))?;
//         for (name, len) in &self.arguments {
//             f.write_fmt(format_args!(", qubit[{}] {}", len, name))?;
//         }
//         f.write_str(" {\n")?;

//         for gate in &self.gates {
//             f.write_str("  ")?;
//             gate.format_qasm(f, &map)?;
//             f.write_str(";\n")?;
//         }

//         // f.write_str("  reset ancillas;\n")?;

//         f.write_str("}\n")?;
//         Ok(())
//     }
// }
