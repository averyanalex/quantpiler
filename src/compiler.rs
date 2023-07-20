use std::rc::Rc;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    bitificator::Gate,
    circuit::{Circuit, Qubit, QubitDesc},
};

fn gen_mcx(
    mut target: Qubit,
    args: &FxHashSet<Rc<Gate>>,
    compiled: &mut FxHashMap<Rc<Gate>, Qubit>,
    circ: &mut Circuit,
) -> Qubit {
    let mut normal_args = Vec::new();
    let mut inversed_args = Vec::new();

    for arg in args {
        match arg.as_ref() {
            Gate::Not(not_arg) => {
                compile_rec(not_arg, compiled, circ);
                inversed_args.push(not_arg);
            }
            Gate::And(_) => panic!("AND in AND"),
            _ => {
                compile_rec(arg, compiled, circ);
                normal_args.push(arg);
            }
        }
    }

    let mut controls = Vec::new();
    let mut inversed_controls = Vec::new();

    for inv_arg in inversed_args {
        let mut ctr = compiled.remove(inv_arg).unwrap();
        ctr = circ.x(ctr);
        inversed_controls.push((ctr, inv_arg.clone()));
    }

    for (ctr, _) in inversed_controls.iter() {
        controls.push(ctr);
    }

    for arg in normal_args {
        let ctr = compiled.get(arg).unwrap();
        controls.push(ctr);
    }

    target = circ.mcx(controls, target);

    for (ctr, gate) in inversed_controls {
        let ctr = circ.x(ctr);
        compiled.insert(gate, ctr);
    }

    target
}

fn optimal_xor_alloc_cost(mut variants: Vec<&Rc<Gate>>) -> Rc<Gate> {
    variants.sort_by_key(|v| v.cost());
    variants.reverse();
    variants.pop().unwrap().clone()
}

fn compile_rec(
    gate: &Rc<Gate>,
    // parent: Option<Rc<Gate>>,
    compiled: &mut FxHashMap<Rc<Gate>, Qubit>,
    // parents: &mut FxHashMap<Rc<Gate>, FxHashSet<Rc<Gate>>>,
    circ: &mut Circuit,
) {
    if !compiled.contains_key(gate) {
        let qubit = match gate.as_ref() {
            Gate::Argument { name: _, index: _ } => circ.get_free_qubit(),
            Gate::Constant(val) => {
                let q = circ.get_free_qubit();
                if *val {
                    circ.x(q)
                } else {
                    q
                }
            }
            Gate::Not(arg) => {
                compile_rec(arg, compiled, circ);
                let q = compiled.remove(arg).unwrap();
                circ.x(q)
            }
            Gate::And(args) => {
                let q = circ.get_free_qubit();
                gen_mcx(q, args, compiled, circ)
            }
            Gate::Xor(args) => {
                let mut already_compiled = Vec::new();
                let mut not_yet_compiled = Vec::new();

                for arg in args {
                    if compiled.get(arg).is_some() {
                        already_compiled.push(arg)
                    } else {
                        not_yet_compiled.push(arg)
                    }
                }

                let mut q = match already_compiled.pop() {
                    Some(comp) => compiled.remove(comp).unwrap(),
                    None => {
                        let best = optimal_xor_alloc_cost(not_yet_compiled.clone());
                        let best_pos = not_yet_compiled.iter().position(|a| **a == best).unwrap();
                        let not_comp = not_yet_compiled.swap_remove(best_pos);

                        compile_rec(not_comp, compiled, circ);
                        compiled.remove(not_comp).unwrap()
                    }
                };

                for comp in already_compiled {
                    let ctr = compiled.get(comp).unwrap();
                    q = circ.cx(ctr, q);
                }

                for not_comp in not_yet_compiled {
                    match not_comp.as_ref() {
                        Gate::Constant(_) | Gate::Xor(_) | Gate::Not(_) => {
                            panic!("this in XOR isn't possible")
                        }
                        Gate::And(args) => {
                            q = gen_mcx(q, args, compiled, circ);
                        }
                        Gate::Argument { .. } => {
                            // todo!("{:?}", not_comp);
                            compile_rec(not_comp, compiled, circ);
                            let arg = compiled.get(not_comp).unwrap();
                            q = circ.cx(arg, q);
                        }
                    }
                }

                q
            }
        };
        compiled.insert(gate.clone(), qubit);
    }
}

pub fn compile(
    logic: Vec<Rc<Gate>>,
    args: Vec<(String, u32)>,
    mut parents: FxHashMap<Rc<Gate>, FxHashSet<Rc<Gate>>>,
) -> Circuit {
    let mut circuit = Circuit::new(args.clone());
    let mut compiled = FxHashMap::default();

    for (name, size) in &args {
        for i in 0..*size {
            let arg_qubit = circuit.get_free_qubit();
            circuit.set_qubit_desc(
                &arg_qubit,
                QubitDesc {
                    reg: crate::circuit::QubitRegister::Argument(name.clone()),
                    index: i,
                },
            );
            compiled.insert(
                Rc::new(Gate::Argument {
                    name: name.clone(),
                    index: i,
                }),
                arg_qubit,
            );
        }
    }

    for (index, gate) in logic.iter().enumerate() {
        compile_rec(gate, &mut compiled, &mut circuit);
        let res_q = compiled.remove(gate).unwrap();
        circuit.set_qubit_desc(
            &res_q,
            QubitDesc {
                reg: crate::circuit::QubitRegister::Result,
                index: index as u32,
            },
        );
    }

    circuit
}
