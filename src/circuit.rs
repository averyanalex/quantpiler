use rustc_hash::{FxHashMap, FxHashSet};

#[derive(Debug)]
pub struct GateX {
    controls: FxHashSet<(Qubit, bool)>,
    target: Qubit,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct QubitDesc {
    pub reg: QubitRegister,
    pub index: u32,
}

#[derive(Debug, Default)]
pub struct Circuit {
    pub qubits_count: u32,
    pub gates: Vec<GateX>,
    qubits_map: FxHashMap<Qubit, FxHashSet<QubitDesc>>,
}

impl Circuit {
    pub fn add_qubit_description(&mut self, qubit: Qubit, description: QubitDesc) {
        self.qubits_map
            .entry(qubit)
            .and_modify(|set| {
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
}
