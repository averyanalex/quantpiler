use std::{fmt::Display, iter, str::FromStr};

use egg::*;
use good_lp::{
    constraint, solvers::highs::HighsParallelType, variable, Expression, ProblemVariables,
    Solution, SolverModel, Variable,
};
use indexmap::IndexMap;
use itertools::Itertools;
use rustc_hash::FxHashMap;

use crate::op::Op;

define_language! {
    pub enum Logic {
        // ^ (XOR) logic gate, commutable
        "^" = Xor(Box<[Id]>),
        // & (AND) logic gate, commutable
        "&" = And(Box<[Id]>),
        // ! (NOT) logic gate
        "!" = Not(Id),
        // merge gates into register, useful for return op
        "r" = Register(Box<[Id]>),
        // just constant value
        Const(bool),
        // argument qubit
        Arg(ArgInfo),
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ArgInfo {
    pub name: String,
    pub index: u32,
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

fn make_rules() -> Vec<Rewrite<Logic, LogicConstantFolding>> {
    use egg::rewrite as rw;
    vec![
        // Commutable
        rw!("comm-xor"; "(^ ?a ?b)" => "(^ ?b ?a)"),
        rw!("comm-and"; "(& ?a ?b)" => "(& ?b ?a)"),
        // Associative
        rw!("assoc-xor"; "(^ ?a (^ ?b ?c))" => "(^ (^ ?a ?b) ?c)"),
        rw!("assoc-and"; "(& ?a (& ?b ?c))" => "(& (& ?a ?b) ?c)"),
        rw!("sus-and"; "(& ?a)" => "?a"),
        rw!("sus-xor"; "(^ ?a)" => "?a"),
        // // Logic with constants
        rw!("xor-with-true"; "(^ ?a true)" => "(! ?a)"),
        rw!("xor-with-false"; "(^ ?a false)" => "?a"),
        rw!("and-with-true"; "(& ?a true)" => "?a"),
        rw!("and-with-false"; "(& ?a false)" => "false"),
        // Same elements logic
        rw!("and-same"; "(& ?a ?a)" => "?a"),
        rw!("and-same-not"; "(& (! ?a) ?a)" => "false"),
        rw!("xor-same"; "(^ ?a ?a)" => "false"),
        rw!("xor-same-not"; "(^ (! ?a) ?a)" => "true"),
        // Etc
        rw!("cancel-not"; "(! (! ?a))" => "?a"),
        rw!("and-xor-and"; "(& ?a (^ ?a ?b))" => "(& ?a (! ?b))"),
        // rw!("create-sus"; "(& ?a (! ?b))" => "(& ?a (^ ?a ?b))"),
        // rw!("create-not-not"; "?a" => "(! (! ?a))"),
        rw!("xor-not-not-xor"; "(^ (! ?a) ?b)" => "(! (^ ?a ?b))"),
        rw!("cancel-xor-not-not"; "(^ (! ?a) (! ?b))" => "(^ ?a ?b)"),
        // rw!("create-xor-not-not"; "(^ ?a ?b)" => "(^ (! ?a) (! ?b))"),
        rw!("not-xor-xor-not"; "(! (^ ?a ?b))" => "(^ (! ?a) ?b)"),
    ]
}

struct _ClassVars {
    active: Variable,
    // order: Col,
    nodes: Vec<Variable>,
}

fn _extract_v2(egraph: &EGraph<Logic, LogicConstantFolding>, root: Id) -> RecExpr<Logic> {
    let mut problem = ProblemVariables::new();
    let mut constraints = Vec::new();

    let vars: IndexMap<Id, _ClassVars> = egraph
        .classes()
        .map(|class| {
            let cvars = _ClassVars {
                active: problem.add(variable().binary()),
                // order: model.add_col(),
                nodes: class
                    .nodes
                    .iter()
                    .map(|_| problem.add(variable().binary()))
                    .collect(),
            };
            // model.set_col_upper(cvars.order, max_order);
            (class.id, cvars)
        })
        .collect();

    for (id, class) in &vars {
        let active_nodes_in_class = class
            .nodes
            .iter()
            .fold(Expression::from(0), |acc, active| acc + active);

        constraints.push((active_nodes_in_class - class.active).eq(0));

        for (node, &node_active) in egraph[*id].nodes.iter().zip(&class.nodes) {
            for child in node.children() {
                let child_active = vars[child].active;
                constraints.push(constraint!(node_active - child_active <= 0));
            }
        }
    }

    constraints.push(constraint!(vars[&root].active == 1));

    let cost = vars
        .iter()
        .map(|(_, vars)| {
            vars.nodes
                .iter()
                .fold(Expression::from(0), |acc, active| acc + active)
        })
        .fold(Expression::from(0), |acc, active| acc + active);

    let problem = constraints.into_iter().fold(
        problem.minimise(cost).using(good_lp::default_solver),
        |acc, constraint| acc.with(constraint),
    );

    let mut problem = problem.set_parallel(HighsParallelType::On).set_threads(16);
    problem.set_verbose(true);

    let solution = problem.solve().unwrap();

    let mut choices = FxHashMap::default();

    for class in egraph.classes() {
        let Some((logic, _)) = class
            .nodes
            .iter()
            .zip(vars[&class.id].nodes.iter())
            .find(|(_, active)| solution.value(**active) == 1.0) else {continue;};
        choices.insert(class.id, logic);
    }

    println!("sus {:?}", choices);

    let get_first_enode = |id| choices[&id].clone();
    let expr = get_first_enode(root).build_recexpr(get_first_enode);
    println!("sus2");
    expr
}

#[derive(Default)]
struct LogicConstantFolding;
impl Analysis<Logic> for LogicConstantFolding {
    type Data = Option<bool>;

    fn merge(&mut self, to: &mut Self::Data, from: Self::Data) -> DidMerge {
        egg::merge_max(to, from)
    }

    fn make(egraph: &EGraph<Logic, Self>, enode: &Logic) -> Self::Data {
        let x = |i: &Id| egraph[*i].data;
        match enode {
            Logic::Xor(args) => args
                .iter()
                .map(|arg| x(arg))
                .fold_options(false, |acc, arg| acc ^ arg),
            Logic::And(args) => args
                .iter()
                .map(|arg| x(arg))
                .fold_options(true, |acc, arg| acc & arg),
            Logic::Not(a) => Some(!x(a)?),
            Logic::Const(a) => Some(*a),
            _ => None,
        }
    }

    fn modify(egraph: &mut EGraph<Logic, Self>, id: Id) {
        if let Some(i) = egraph[id].data {
            let added = egraph.add(Logic::Const(i));
            egraph.union(id, added);
        }
    }
}

pub struct XorMinimizerCost;

// impl CostFunction<Logic> for XorMinimizerCost {
//     type Cost = usize;

//     fn cost<C>(&mut self, enode: &Logic, mut costs: C) -> Self::Cost
//     where
//         C: FnMut(Id) -> Self::Cost,
//     {
//         match enode {
//             Logic::Xor(..) => enode.fold(16, |sum, i| sum.saturating_add(costs(i))),
//             Logic::And(..) => enode.fold(4, |sum, i| sum.saturating_add(costs(i))),
//             Logic::Not(..) => enode.fold(1, |sum, i| sum.saturating_add(costs(i))),
//             Logic::Register(..) => enode.fold(0, |sum, i| sum.saturating_add(costs(i))),
//             Logic::Const(..) => 0,
//             Logic::Arg(..) => 0,
//         }
//     }
// }

impl LpCostFunction<Logic, LogicConstantFolding> for XorMinimizerCost {
    fn node_cost(
        &mut self,
        _egraph: &EGraph<Logic, LogicConstantFolding>,
        _eclass: Id,
        enode: &Logic,
    ) -> f64 {
        match enode {
            Logic::Xor(_srcs) => {
                // let a = egraph.id_to_expr(*a);
                // let b = egraph.id_to_expr(*b);

                // egraph.
                512.0
            }
            Logic::And(..) => 32.0,
            Logic::Not(..) => 2.0,
            Logic::Register(..) => 0.1,
            Logic::Const(..) => 0.1,
            Logic::Arg(..) => 0.1,
        }
    }
}

pub struct Logificator {
    egraph: EGraph<Logic, LogicConstantFolding>,
    op_expr: RecExpr<Op>,
    op_cache: FxHashMap<Id, Vec<Id>>,
}

impl Logificator {
    pub fn new(expr: RecExpr<Op>) -> Self {
        Self {
            egraph: EGraph::new(LogicConstantFolding),
            op_expr: expr,
            op_cache: FxHashMap::default(),
        }
    }

    pub fn build_logic(mut self) -> RecExpr<Logic> {
        let return_ids = self.get_logificated(Id::from(self.op_expr.as_ref().len() - 1));
        let return_logic = Logic::Register(return_ids.into_boxed_slice());
        let return_id = self.egraph.add(return_logic);

        let mut runner = Runner::default()
            .with_egraph(self.egraph)
            .with_time_limit(std::time::Duration::from_secs(3600))
            .with_node_limit(1000)
            .with_iter_limit(20);
        runner.roots.push(return_id);

        runner = runner.run(&make_rules());

        // extract_v2(&runner.egraph, runner.roots[0]);

        let expr = crate::extract::extract(&runner.egraph, runner.roots[0], XorMinimizerCost);

        let xors = expr
            .as_ref()
            .iter()
            .filter(|b| matches!(b, Logic::Xor(..)))
            .count();

        println!("Simplified to len {}, xors: {}", expr.as_ref().len(), xors);

        let mut gr = EGraph::new(());
        gr.add_expr(&expr);
        gr.dot().to_dot("bits.dot").unwrap();

        expr
    }

    fn get_logificated(&mut self, id: Id) -> Vec<Id> {
        match self.op_cache.get(&id) {
            Some(bits) => bits.clone(),
            None => {
                let op = self.op_expr[id].clone();
                let ids: Vec<_> = match op {
                    Op::Argument(argument) => (0..(argument.size))
                        .map(|index| {
                            Logic::Arg(ArgInfo {
                                name: argument.name.clone(),
                                index,
                            })
                        })
                        .map(|l| self.egraph.add(l))
                        .collect(),
                    Op::Ternary([cond, then, or]) => {
                        let cond = self.get_logificated(cond)[0];
                        let inv_cond = self.egraph.add(Logic::Not(cond));

                        self.get_logificated(then)
                            .into_iter()
                            .zip_longest(self.get_logificated(or).into_iter())
                            .map(|thenor| match thenor {
                                itertools::EitherOrBoth::Both(then, or) => {
                                    let then_cond =
                                        self.egraph.add(Logic::And(Box::new([then, cond])));
                                    let or_inv_cond =
                                        self.egraph.add(Logic::And(Box::new([or, inv_cond])));
                                    self.egraph
                                        .add(Logic::Xor(Box::new([then_cond, or_inv_cond])))
                                }
                                itertools::EitherOrBoth::Left(then) => {
                                    self.egraph.add(Logic::And(Box::new([then, cond])))
                                }
                                itertools::EitherOrBoth::Right(or) => {
                                    self.egraph.add(Logic::And(Box::new([or, inv_cond])))
                                }
                            })
                            .collect()
                    }
                    Op::Constant(value) => (0..64)
                        .map(|i| ((u64::try_from(value.clone()).unwrap() >> i) & 1) == 1)
                        .rev()
                        .collect_vec()
                        .into_iter()
                        .skip_while(|x| !x)
                        .map(Logic::Const)
                        .map(|l| self.egraph.add(l))
                        .collect_vec()
                        .into_iter()
                        .rev()
                        .collect_vec(),
                    Op::Not(a) => self
                        .get_logificated(a)
                        .into_iter()
                        .map(Logic::Not)
                        .map(|l| self.egraph.add(l))
                        .collect(),
                    Op::Xor([a, b]) => self
                        .get_logificated(a)
                        .into_iter()
                        .zip_longest(self.get_logificated(b).into_iter())
                        .map(|ab| match ab {
                            itertools::EitherOrBoth::Both(a, b) => {
                                self.egraph.add(Logic::Xor(Box::new([a, b])))
                            }
                            itertools::EitherOrBoth::Left(a)
                            | itertools::EitherOrBoth::Right(a) => a,
                        })
                        .collect(),
                    Op::Or([a, b]) => self
                        .get_logificated(a)
                        .into_iter()
                        .zip_longest(self.get_logificated(b).into_iter())
                        .map(|ab| match ab {
                            itertools::EitherOrBoth::Both(a, b) => {
                                let not_a = self.egraph.add(Logic::Not(a));
                                let not_b = self.egraph.add(Logic::Not(b));
                                let not_a_not_b =
                                    self.egraph.add(Logic::And(Box::new([not_a, not_b])));
                                self.egraph.add(Logic::Not(not_a_not_b))
                            }
                            itertools::EitherOrBoth::Left(a)
                            | itertools::EitherOrBoth::Right(a) => a,
                        })
                        .collect(),
                    Op::And([a, b]) => self
                        .get_logificated(a)
                        .into_iter()
                        .zip(self.get_logificated(b))
                        .map(|(a, b)| Logic::And(Box::new([a, b])))
                        .map(|l| self.egraph.add(l))
                        .collect(),
                    Op::Shr([target, distance]) => {
                        let Op::Constant(distance) = self.op_expr[distance].clone() else {todo!()};
                        self.get_logificated(target)
                            .into_iter()
                            .skip(distance.try_into().unwrap())
                            .collect()
                    }
                    Op::Shl([target, distance]) => {
                        let Op::Constant(distance) = self.op_expr[distance].clone() else {todo!()};
                        iter::repeat(self.egraph.add(Logic::Const(false)))
                            .take(distance.try_into().unwrap())
                            .chain(self.get_logificated(target).into_iter())
                            .collect()
                    }
                    Op::Add([a, b]) => {
                        let mut c = None;
                        let mut bits: Vec<_> = self
                            .get_logificated(a)
                            .into_iter()
                            .zip_longest(self.get_logificated(b).into_iter())
                            .map(|ab| match ab {
                                itertools::EitherOrBoth::Both(a, b) => {
                                    let a_xor_b = self.egraph.add(Logic::Xor(Box::new([a, b])));
                                    let a_and_b = self.egraph.add(Logic::And(Box::new([a, b])));
                                    if let Some(cin) = c {
                                        let cin_and_axorb =
                                            self.egraph.add(Logic::And(Box::new([cin, a_xor_b])));
                                        c =
                                            Some(self.egraph.add(Logic::Xor(Box::new([
                                                a_and_b,
                                                cin_and_axorb,
                                            ]))));
                                        self.egraph.add(Logic::Xor(Box::new([a_xor_b, cin])))
                                    } else {
                                        c = Some(a_and_b);
                                        a_xor_b
                                    }
                                }
                                itertools::EitherOrBoth::Left(a) => {
                                    if let Some(cin) = c {
                                        c = Some(self.egraph.add(Logic::And(Box::new([cin, a]))));
                                        self.egraph.add(Logic::Xor(Box::new([cin, a])))
                                    } else {
                                        a
                                    }
                                }
                                itertools::EitherOrBoth::Right(b) => {
                                    if let Some(cin) = c {
                                        c = Some(self.egraph.add(Logic::And(Box::new([cin, b]))));
                                        self.egraph.add(Logic::Xor(Box::new([cin, b])))
                                    } else {
                                        b
                                    }
                                }
                            })
                            .collect();
                        if let Some(c) = c {
                            bits.push(c)
                        }
                        bits
                    }
                    _ => todo!(),
                };

                self.op_cache.insert(id, ids.clone());
                ids
            }
        }
    }
}
