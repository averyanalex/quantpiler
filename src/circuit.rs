#[cfg(feature = "python")]
use std::collections::hash_map::Entry;

#[cfg(feature = "python")]
use itertools::Itertools;
#[cfg(feature = "python")]
use pyo3::{exceptions::PyValueError, prelude::*};
use rustc_hash::{FxHashMap, FxHashSet};

#[derive(Debug, Clone)]
#[cfg_attr(feature = "python", pyclass(get_all))]
pub struct GateX {
    controls: FxHashSet<(Qubit, bool)>,
    target: Qubit,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
#[cfg_attr(feature = "python", pyclass(get_all))]
pub struct Qubit {
    pub index: u32,
}

impl Qubit {
    pub const fn new(index: u32) -> Self {
        Self { index }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum QubitRegisterEnum {
    Ancillary,
    Result,
    Argument(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "python", pyclass)]
pub struct QubitRegister(pub QubitRegisterEnum);

#[cfg(feature = "python")]
#[pymethods]
impl QubitRegister {
    const fn is_result(&self) -> bool {
        matches!(self.0, QubitRegisterEnum::Result)
    }

    const fn is_ancillary(&self) -> bool {
        matches!(self.0, QubitRegisterEnum::Ancillary)
    }

    const fn is_argument(&self) -> bool {
        matches!(self.0, QubitRegisterEnum::Argument(..))
    }

    fn argument_name(&self) -> PyResult<String> {
        match &self.0 {
            QubitRegisterEnum::Argument(name) => Ok(name.clone()),
            _ => Err(PyValueError::new_err("register isn't argument")),
        }
    }

    fn name(&self) -> String {
        match &self.0 {
            QubitRegisterEnum::Ancillary => "anc".to_owned(),
            QubitRegisterEnum::Result => "ret".to_owned(),
            QubitRegisterEnum::Argument(name) => name.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "python", pyclass(get_all))]
pub struct QubitDesc {
    pub reg: QubitRegister,
    pub idx: usize,
}

#[derive(Debug, Default)]
#[cfg_attr(feature = "python", pyclass(get_all))]
pub struct Circuit {
    pub qubits_count: u32,
    pub gates: Vec<GateX>,
    qubits_map: FxHashMap<Qubit, FxHashSet<QubitDesc>>,
}

#[cfg(feature = "python")]
#[pymethods]
impl Circuit {
    fn qubits_map_list(&self) -> Vec<(Qubit, FxHashSet<QubitDesc>)> {
        let mut map = self.qubits_map.clone();

        for descs in map.values() {
            for desc in descs {
                assert!(!desc.reg.is_ancillary());
            }
        }

        let mut ancilla_idx = 0;

        for i in 0..self.qubits_count {
            if let Entry::Vacant(e) = map.entry(Qubit::new(i)) {
                let mut set = FxHashSet::default();
                set.insert(QubitDesc {
                    reg: QubitRegister(QubitRegisterEnum::Ancillary),
                    idx: ancilla_idx,
                });
                ancilla_idx += 1;
                e.insert(set);
            }
        }

        let mut list = map.into_iter().collect_vec();
        list.sort_unstable_by_key(|x| x.0.index);

        for (idx, (q, _)) in list.iter().enumerate() {
            assert_eq!(idx, q.index as usize);
        }

        list
    }
}

impl Circuit {
    pub fn add_qubit_description(&mut self, qubit: Qubit, description: QubitDesc) {
        self.qubits_map
            .entry(qubit)
            .or_default()
            .insert(description);
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
            controls: std::iter::once((source, inversed)).collect(),
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
                if let QubitRegisterEnum::Argument(arg) = value.reg.clone().0 {
                    qubits.insert(*qubit, args[&arg][value.idx]);
                }
            }
        }

        for gate in &self.gates {
            qubits.insert(
                gate.target,
                qubits.get(&gate.target).unwrap_or(&false)
                    ^ gate.controls.iter().fold(true, |acc, (qubit, inverted)| {
                        acc & (qubits.get(qubit).unwrap_or(&false) ^ inverted)
                    }),
            );
        }

        let mut result = vec![
            false;
            self.qubits_map
                .values()
                .map(|set| set
                    .iter()
                    .map(|desc| if desc.reg.0 == QubitRegisterEnum::Result {
                        desc.idx + 1
                    } else {
                        0
                    })
                    .max()
                    .unwrap())
                .max()
                .unwrap()
        ];

        for (qubit, values) in &self.qubits_map {
            for value in values {
                if value.reg.0 == QubitRegisterEnum::Result {
                    result[value.idx] = *qubits.get(qubit).unwrap_or(&false);
                }
            }
        }

        result
    }
}
