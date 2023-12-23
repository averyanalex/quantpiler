use egg::{Id, Language, RecExpr};
use itertools::Itertools;
use petgraph::prelude::*;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    circuit::{Circuit, Qubit, QubitDesc, QubitRegister, QubitRegisterEnum},
    logic::Logic,
};

pub struct Compiler {
    circuit: Circuit,
    graph: DiGraph<LogicNode, LogicEdge>,
    root_node: NodeIndex,
}

impl Compiler {
    pub fn new(logic: &RecExpr<Logic>) -> Self {
        let mut args = FxHashMap::default();
        for logic_op in logic.as_ref() {
            if let Logic::Arg(arg) = logic_op {
                args.entry(arg.name.clone())
                    .and_modify(|a| {
                        *a = (arg.index + 1).max(*a);
                    })
                    .or_insert(arg.index + 1);
            }
        }

        let mut circuit = Circuit::default();
        let mut graph = Graph::new();
        let mut node_ids = FxHashMap::default();

        let mut root = None;

        let args_qubits: FxHashMap<_, _> = args
            .iter()
            .map(|(name, len)| {
                let qubits = (0..(*len))
                    .map(|i| {
                        let q = circuit.get_ancilla_qubit();
                        circuit.add_qubit_description(
                            q,
                            QubitDesc {
                                reg: QubitRegister(QubitRegisterEnum::Argument(name.clone())),
                                index: i,
                            },
                        );
                        q
                    })
                    .collect_vec();
                (name.clone(), qubits)
            })
            .collect();

        for (parent_id, parent) in logic.as_ref().iter().enumerate() {
            let kind = match parent {
                Logic::Xor(_) => LogicNodeKind::Xor,
                Logic::And(_) => LogicNodeKind::And,
                Logic::Not(_) => LogicNodeKind::Not,
                Logic::Register(_) => LogicNodeKind::Register,
                Logic::Const(value) => LogicNodeKind::Constant(*value),
                Logic::Arg(_) => LogicNodeKind::Arg,
            };
            let graph_parent_id = graph.add_node(LogicNode {
                kind,
                qubit: if let Logic::Arg(arg) = parent {
                    Some(args_qubits[&arg.name][arg.index as usize])
                } else {
                    None
                },
            });
            if parent_id == logic.as_ref().len() - 1 {
                root = Some(graph_parent_id);
            }
            node_ids.insert(Id::from(parent_id), graph_parent_id);
            for child in parent.children() {
                graph.add_edge(node_ids[child], graph_parent_id, LogicEdge { done: false });
            }
        }

        // dbg!(petgraph::dot::Dot::with_config(&graph, &[petgraph::dot::Config::EdgeNoLabel]));

        Self {
            circuit,
            graph,
            root_node: root.unwrap(),
        }
    }

    /// Count undone outgoing edges
    fn count_undone_dependents(&self, node: NodeIndex) -> usize {
        self.graph
            .edges_directed(node, Direction::Outgoing)
            .filter(|edge| !edge.weight().done)
            .count()
    }

    /// Check if we can use node as dependency
    fn is_node_available(&self, node_id: NodeIndex) -> bool {
        let node = &self.graph[node_id];

        (node.qubit.is_some()
            && self
                .graph
                .edges_directed(node_id, Direction::Incoming)
                .all(|e| e.weight().done))
            || match node.kind {
                LogicNodeKind::Xor | LogicNodeKind::Arg => false, // only can use XOR/Arg if qubit.is_some()
                #[allow(clippy::match_same_arms)]
                LogicNodeKind::Constant(_) => false, // there should be no gates dependent on constants
                LogicNodeKind::Register | LogicNodeKind::And | LogicNodeKind::Not => self
                    .graph
                    .neighbors_directed(node_id, Direction::Incoming)
                    .all(|src_id| self.is_node_available(src_id)),
            }
    }

    /// Recursively make edge done
    fn make_edge_done(&mut self, edge: EdgeIndex) {
        if self.graph[edge].done {
            return;
        }

        self.graph[edge].done = true;
        let (source, target) = self.graph.edge_endpoints(edge).unwrap();

        if self.graph[target].kind == LogicNodeKind::And {
            let edges = self
                .graph
                .edges_directed(target, Direction::Incoming)
                .map(|edge| edge.id())
                .collect_vec();

            for edge in edges {
                self.make_edge_done(edge);
            }
        }

        if self.count_undone_dependents(source) == 0 {
            let edges = self
                .graph
                .edges_directed(source, Direction::Incoming)
                .map(|edge| edge.id())
                .collect_vec();

            for edge in edges {
                self.make_edge_done(edge);
            }
        }
    }

    fn construct_mcx(&mut self, and: NodeIndex, target: Qubit) {
        fn collect_sources_of_and(
            and: NodeIndex,
            graph: &DiGraph<LogicNode, LogicEdge>,
            mcx_sources: &mut FxHashSet<(Qubit, bool)>,
        ) {
            for source in graph.neighbors_directed(and, Direction::Incoming) {
                if let Some(source_qubit) = graph[source].qubit {
                    mcx_sources.insert((source_qubit, false));
                } else {
                    match graph[source].kind {
                        LogicNodeKind::And => collect_sources_of_and(source, graph, mcx_sources),
                        LogicNodeKind::Not => {
                            let arg_of_not = graph
                                .neighbors_directed(source, Direction::Incoming)
                                .next()
                                .unwrap();

                            assert_ne!(graph[arg_of_not].kind, LogicNodeKind::Not);

                            mcx_sources.insert((graph[arg_of_not].qubit.unwrap(), true));
                        }
                        LogicNodeKind::Arg
                        | LogicNodeKind::Xor
                        | LogicNodeKind::Register
                        | LogicNodeKind::Constant(_) => unreachable!(),
                    }
                }
            }
        }

        let mut mcx_sources = FxHashSet::default();

        collect_sources_of_and(and, &self.graph, &mut mcx_sources);

        // dbg!(&mcx_sources);

        self.circuit.mcx(mcx_sources, target);
    }

    fn execute_edge(&mut self, edge: EdgeIndex, target_qubit: Qubit) {
        let (source, target) = self.graph.edge_endpoints(edge).unwrap();

        match self.graph[target].kind {
            LogicNodeKind::Xor => {
                if let Some(source_qubit) = self.graph[source].qubit {
                    self.circuit.cx(source_qubit, false, target_qubit);
                } else {
                    match self.graph[source].kind {
                        LogicNodeKind::Not => {
                            let arg_of_not = self
                                .graph
                                .neighbors_directed(source, Direction::Incoming)
                                .next()
                                .unwrap();

                            assert_ne!(self.graph[arg_of_not].kind, LogicNodeKind::Not);

                            if let Some(arg_of_not_qubit) = self.graph[arg_of_not].qubit {
                                self.circuit.cx(arg_of_not_qubit, true, target_qubit);
                            } else {
                                match self.graph[arg_of_not].kind {
                                    LogicNodeKind::And => {
                                        self.construct_mcx(arg_of_not, target_qubit);
                                        self.circuit.x(target_qubit);
                                    }
                                    LogicNodeKind::Xor
                                    | LogicNodeKind::Arg
                                    | LogicNodeKind::Constant(_)
                                    | LogicNodeKind::Not
                                    | LogicNodeKind::Register => unreachable!(),
                                }
                            }
                        }
                        LogicNodeKind::And => {
                            self.construct_mcx(source, target_qubit);
                        }
                        LogicNodeKind::Xor
                        | LogicNodeKind::Arg
                        | LogicNodeKind::Constant(..)
                        | LogicNodeKind::Register => {
                            unreachable!()
                        }
                    }
                }
            }
            LogicNodeKind::And => {
                self.construct_mcx(target, target_qubit);
            }
            LogicNodeKind::Not => match self.graph[source].qubit {
                Some(qubit) => {
                    self.circuit.cx(qubit, true, target_qubit);
                }
                None => {
                    if matches!(self.graph[source].kind, LogicNodeKind::And) {
                        self.construct_mcx(target, target_qubit);
                        self.circuit.x(target_qubit);
                    } else {
                        todo!("{:?}", self.graph[source].kind);
                    }
                }
            },
            LogicNodeKind::Arg => todo!(),
            LogicNodeKind::Register => todo!(),
            LogicNodeKind::Constant(_) => todo!(),
        }
    }

    pub fn compile(mut self) -> Circuit {
        // let mut allocs = 0u32;
        // let mut optimal = 0u32;

        for node in self.graph.node_weights_mut() {
            if let LogicNodeKind::Constant(value) = node.kind {
                let q = self.circuit.get_ancilla_qubit();
                if value {
                    self.circuit.x(q);
                }
                node.qubit = Some(q);
            }
        }

        while !self
            .graph
            .neighbors_directed(self.root_node, Direction::Incoming)
            .all(|node| {
                self.graph[node].qubit.is_some()
                    && self
                        .graph
                        .edges_directed(node, Direction::Incoming)
                        .all(|e| e.weight().done)
            })
        {
            // collect vector with available, undone edges
            let available_edges = self
                .graph
                .edge_references()
                .filter(|edge| !edge.weight().done) // skip already done edges
                .filter(|edge| {
                    match self.graph[edge.target()].kind {
                        LogicNodeKind::Xor | LogicNodeKind::Not | LogicNodeKind::Register => self.is_node_available(edge.source()),
                        LogicNodeKind::And => self.graph.neighbors_directed(edge.target(), Direction::Incoming).all(|n| self.is_node_available(n)),
                        LogicNodeKind::Arg | LogicNodeKind::Constant(_) => unreachable!(),
                    }
        }) // only edges available to process
                .filter(|edge| {
                    self.graph
                        .neighbors_directed(edge.target(), Direction::Outgoing)
                        .any(|trg| self.graph[trg].kind == LogicNodeKind::Register) // need all nodes used in register
                        || match self.graph[edge.target()].kind {
                            LogicNodeKind::Xor | LogicNodeKind::Register => true, // construct XORs and registers always
                            LogicNodeKind::Arg | LogicNodeKind::Constant(..) => unreachable!(), // there can't be edges to arg/const
                            LogicNodeKind::Not => false, // can use inversed control in QASM
                            LogicNodeKind::And => self // "a" needed only if there is AND(NOT(a), ...)
                                .graph
                                .edges_directed(edge.target(), Direction::Outgoing)
                                .any(|a_dependent| {
                                    !a_dependent.weight().done &&
                                    self.graph[a_dependent.target()].kind == LogicNodeKind::Not
                                        && self
                                            .graph
                                            .edges_directed(a_dependent.target(), Direction::Outgoing)
                                            .any(|not_dependent| !not_dependent.weight().done
                                                && self.graph[not_dependent.target()].kind == LogicNodeKind::And)
                                }),
                        }
                })
                .map(|edge| edge.id())
                .collect_vec();

            let mut did_something_optimal = false;

            for edge in &available_edges {
                let (source, target) = self.graph.edge_endpoints(*edge).unwrap();

                if let Some(target_qubit) = self.graph[target].qubit {
                    assert_eq!(self.graph[target].kind, LogicNodeKind::Xor);

                    self.execute_edge(*edge, target_qubit);
                    self.make_edge_done(*edge);

                    did_something_optimal = true;
                    // optimal += 1;
                } else if self.count_undone_dependents(source) == 1
                    && self.graph[target].kind == LogicNodeKind::Xor
                    && let Some(source_qubit) = self.graph[source].qubit
                {
                    assert!(self.graph[target].qubit.is_none());

                    self.graph[target].qubit = Some(source_qubit);
                    self.graph[source].qubit = None;

                    self.make_edge_done(*edge);

                    did_something_optimal = true;
                    // optimal += 1;
                } else if self.count_undone_dependents(source) == 1
                    && self.graph[target].kind == LogicNodeKind::Not
                    && let Some(source_qubit) = self.graph[source].qubit
                {
                    assert!(self.graph[target].qubit.is_none());

                    self.graph[target].qubit = Some(source_qubit);
                    self.graph[source].qubit = None;

                    self.circuit.x(source_qubit);

                    self.make_edge_done(*edge);

                    did_something_optimal = true;
                    // optimal += 1;
                }
            }

            if !did_something_optimal {
                let Some(edge) = available_edges.first() else {
                    panic!("can't compile")
                };
                let (_, target) = self.graph.edge_endpoints(*edge).unwrap();

                assert!(self.graph[target].qubit.is_none()); // unoptimal => allocation
                let target_qubit = self.circuit.get_ancilla_qubit();

                self.execute_edge(*edge, target_qubit);
                self.make_edge_done(*edge);

                self.graph[target].qubit = Some(target_qubit);

                // allocs += 1;
            }
        }

        for (idx, node) in self
            .graph
            .neighbors_directed(self.root_node, Direction::Incoming)
            .collect_vec()
            .into_iter()
            .rev()
            .enumerate()
        {
            self.circuit.add_qubit_description(
                self.graph[node].qubit.unwrap(),
                QubitDesc {
                    reg: QubitRegister(QubitRegisterEnum::Result),
                    index: idx as u32,
                },
            );
        }

        // println!("Allocs: {allocs}, optimal: {optimal}");

        self.circuit
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum LogicNodeKind {
    Xor,
    And,
    Not,
    Arg,
    Register,
    Constant(bool),
}

#[derive(Debug)]
struct LogicNode {
    qubit: Option<Qubit>,
    kind: LogicNodeKind,
}

#[derive(Debug)]
struct LogicEdge {
    done: bool,
}
