use std::{fmt::Display, iter, str::FromStr};

use egg::*;
use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{extract::LpCostFunction, op::Op};

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

impl Logic {
    fn optimize(&self, egraph: &EGraph<Logic, LogicConstantFolding>) -> Self {
        match self {
            Logic::Xor(args) => {
                assert!(!args.is_empty());

                let mut powerful_xor_args = FxHashSet::default();
                let mut parents = FxHashSet::default();
                fn collect_xor_args(
                    args: &[Id],
                    _egraph: &EGraph<Logic, LogicConstantFolding>,
                    powerful_xor_args: &mut FxHashSet<Id>,
                    parents: &mut FxHashSet<Id>,
                ) -> Option<()> {
                    for arg in args.iter() {
                        parents.insert(*arg);
                        // if let Logic::Xor(inner_args) = &egraph[*arg].data.optimized {
                        //     if inner_args.iter().any(|a| parents.contains(a)) {
                        //         // panic!("infinite recursion detected");
                        //         return None;
                        //     }
                        //     collect_xor_args(inner_args, egraph, powerful_xor_args, parents);
                        // } else if powerful_xor_args.contains(arg) {
                        if powerful_xor_args.contains(arg) {
                            powerful_xor_args.remove(arg);
                        } else {
                            powerful_xor_args.insert(*arg);
                        }
                    }
                    Some(())
                }
                if collect_xor_args(args, egraph, &mut powerful_xor_args, &mut parents).is_some() {
                    if powerful_xor_args.is_empty() {
                        Logic::Const(false)
                    } else if powerful_xor_args.len() == 1 {
                        egraph[powerful_xor_args.into_iter().next().unwrap()]
                            .data
                            .optimized
                            .clone()
                    } else {
                        Logic::Xor(powerful_xor_args.into_iter().collect())
                    }
                } else {
                    self.clone()
                }
            }
            Logic::And(args) => {
                assert!(!args.is_empty());

                let mut unique_and_args = FxHashSet::default();
                let mut parents = FxHashSet::default();
                fn collect_and_args(
                    args: &[Id],
                    egraph: &EGraph<Logic, LogicConstantFolding>,
                    unique_and_args: &mut FxHashSet<Id>,
                    parents: &mut FxHashSet<Id>,
                ) -> Option<()> {
                    for arg in args.iter() {
                        match &egraph[*arg].data.optimized {
                            Logic::And(inner_args) => {
                                if inner_args.iter().any(|a| parents.contains(a)) {
                                    // panic!("infinite recursion detected");
                                    return None;
                                }

                                collect_and_args(inner_args, egraph, unique_and_args, parents)?;
                            }
                            Logic::Const(true) => {}
                            _ => {
                                unique_and_args.insert(*arg);
                            }
                        }
                    }
                    Some(())
                }

                if collect_and_args(args, egraph, &mut unique_and_args, &mut parents).is_some() {
                    if unique_and_args.is_empty() {
                        // the only arg was in and is true
                        Logic::Const(true)
                    } else if unique_and_args.len() == 1 {
                        egraph[unique_and_args.into_iter().next().unwrap()]
                            .data
                            .optimized
                            .clone()
                    } else if unique_and_args
                        .iter()
                        .any(|a| egraph[*a].data.optimized == Logic::Const(false))
                    {
                        Logic::Const(false)
                    } else {
                        Logic::And(unique_and_args.into_iter().collect())
                    }
                } else {
                    self.clone()
                }
            }
            Logic::Not(arg) => match egraph[*arg].data.optimized.clone() {
                Logic::Not(arg_in_not) => egraph[arg_in_not].data.optimized.clone(),
                Logic::Const(constant) => Logic::Const(!constant),
                _ => self.clone(),
            },
            _ => self.clone(),
        }
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct LogicFoldingData {
    value: Option<bool>,
    optimized: Logic,
}

#[derive(Default)]
struct LogicConstantFolding;
impl Analysis<Logic> for LogicConstantFolding {
    type Data = LogicFoldingData;

    fn merge(&mut self, to: &mut Self::Data, from: Self::Data) -> DidMerge {
        egg::merge_max(to, from)
    }

    fn make(egraph: &EGraph<Logic, Self>, enode: &Logic) -> Self::Data {
        let xc = |i: &Id| egraph[*i].data.value;

        let make_value = || match enode {
            Logic::Xor(args) => args
                .iter()
                .map(|arg| xc(arg))
                .fold_options(false, |acc, arg| acc ^ arg),
            Logic::And(args) => args
                .iter()
                .map(|arg| xc(arg))
                .fold_options(true, |acc, arg| acc & arg),
            Logic::Not(a) => Some(!xc(a)?),
            Logic::Const(a) => Some(*a),
            Logic::Register(_) | Logic::Arg(_) => None,
        };

        let mut value = make_value();
        let optimized = enode.optimize(egraph);

        if let Logic::Const(c) = &optimized {
            if let Some(v) = value {
                assert_eq!(v, *c);
            } else {
                value = Some(*c)
            }
        }

        LogicFoldingData { value, optimized }
    }

    fn modify(egraph: &mut EGraph<Logic, Self>, id: Id) {
        if let Some(i) = egraph[id].data.value {
            let added = egraph.add(Logic::Const(i));
            egraph.union(id, added);
        }
        let added = egraph.add(egraph[id].data.optimized.clone());
        egraph.union(id, added);
    }
}

pub struct XorMinimizerCost;

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

fn build_add(egraph: &mut EGraph<Logic, LogicConstantFolding>, a: &[Id], b: &[Id]) -> Vec<Id> {
    let mut c = None;

    let mut bits = a
        .iter()
        .zip_longest(b.iter())
        .map(|ab| match ab {
            itertools::EitherOrBoth::Both(a, b) => {
                let a_xor_b = egraph.add(Logic::Xor(Box::new([*a, *b])));
                let a_and_b = egraph.add(Logic::And(Box::new([*a, *b])));
                if let Some(cin) = c {
                    let cin_and_axorb = egraph.add(Logic::And(Box::new([cin, a_xor_b])));
                    c = Some(egraph.add(Logic::Xor(Box::new([a_and_b, cin_and_axorb]))));
                    egraph.add(Logic::Xor(Box::new([a_xor_b, cin])))
                } else {
                    c = Some(a_and_b);
                    a_xor_b
                }
            }
            itertools::EitherOrBoth::Left(a) => {
                if let Some(cin) = c {
                    c = Some(egraph.add(Logic::And(Box::new([cin, *a]))));
                    egraph.add(Logic::Xor(Box::new([cin, *a])))
                } else {
                    *a
                }
            }
            itertools::EitherOrBoth::Right(b) => {
                if let Some(cin) = c {
                    c = Some(egraph.add(Logic::And(Box::new([cin, *b]))));
                    egraph.add(Logic::Xor(Box::new([cin, *b])))
                } else {
                    *b
                }
            }
        })
        .collect_vec();
    if let Some(c) = c {
        bits.push(c)
    }
    bits
}

fn build_mul(egraph: &mut EGraph<Logic, LogicConstantFolding>, a: &[Id], b: &[Id]) -> Vec<Id> {
    b.iter()
        .enumerate()
        .map(|(idx, b_bit)| {
            iter::repeat(Logic::Const(false))
                .take(idx)
                .chain(a.iter().map(|a_bit| Logic::And(Box::new([*a_bit, *b_bit]))))
                .map(|l| egraph.add(l))
                .collect_vec()
        })
        .collect_vec()
        .into_iter()
        .fold(vec![], |acc, x| build_add(egraph, &acc, &x))
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
            .with_node_limit(1200)
            .with_iter_limit(20);
        runner.roots.push(return_id);

        runner = runner.run(&make_rules());

        let expr = crate::extract::extract(&runner.egraph, runner.roots[0], XorMinimizerCost);

        // let xors = expr
        //     .as_ref()
        //     .iter()
        //     .filter(|b| matches!(b, Logic::Xor(..)))
        //     .count();

        // println!("Simplified to len {}, xors: {}", expr.as_ref().len(), xors);

        let mut gr = EGraph::new(());
        gr.add_expr(&expr);
        // gr.dot().to_dot("bits.dot").unwrap();

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
                        let a = self.get_logificated(a);
                        let b = self.get_logificated(b);
                        build_add(&mut self.egraph, &a, &b)
                    }
                    Op::Mul([a, b]) => {
                        let a = self.get_logificated(a);
                        let b = self.get_logificated(b);
                        build_mul(&mut self.egraph, &a, &b)
                    }
                    _ => todo!("{op}"),
                };

                self.op_cache.insert(id, ids.clone());
                ids
            }
        }
    }
}
