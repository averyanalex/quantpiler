use std::fmt::Display;

use egg::{Analysis, EGraph, Id, Language, RecExpr};
use egraph_serialize::{ClassId, NodeId};
use good_lp::{
    constraint, solvers::highs::HighsParallelType, variable, Expression, ProblemVariables,
    Solution, SolverModel, Variable,
};
use indexmap::IndexMap;

use indexmap::IndexSet;
use ordered_float::NotNan;

pub trait LpCostFunction<L: Language, N: Analysis<L>> {
    /// Returns the cost of the given e-node.
    ///
    /// This function may look at other parts of the e-graph to compute the cost
    /// of the given e-node.
    fn node_cost(&mut self, egraph: &EGraph<L, N>, eclass: Id, enode: &L) -> f64;
}

// Most of this code was taken from https://github.com/egraphs-good/extraction-gym/tree/main
type Cost = NotNan<f64>;

trait Extractor: Sync {
    fn extract(&self, egraph: &egraph_serialize::EGraph, roots: &[ClassId]) -> ExtractionResult;

    fn boxed(self) -> Box<dyn Extractor>
    where
        Self: Sized + 'static,
    {
        Box::new(self)
    }
}

#[derive(Default, Clone)]
struct ExtractionResult {
    pub choices: IndexMap<ClassId, NodeId>,
}

#[derive(Clone, Copy)]
enum Status {
    Doing,
    Done,
}

impl ExtractionResult {
    fn choose(&mut self, class_id: ClassId, node_id: NodeId) {
        self.choices.insert(class_id, node_id);
    }

    fn find_cycles(&self, egraph: &egraph_serialize::EGraph, roots: &[ClassId]) -> Vec<ClassId> {
        // let mut status = vec![Status::Todo; egraph.classes().len()];
        let mut status = IndexMap::<ClassId, Status>::default();
        let mut cycles = vec![];
        for root in roots {
            // let root_index = egraph.classes().get_index_of(root).unwrap();
            self.cycle_dfs(egraph, root, &mut status, &mut cycles)
        }
        cycles
    }

    fn cycle_dfs(
        &self,
        egraph: &egraph_serialize::EGraph,
        class_id: &ClassId,
        status: &mut IndexMap<ClassId, Status>,
        cycles: &mut Vec<ClassId>,
    ) {
        match status.get(class_id).cloned() {
            Some(Status::Done) => (),
            Some(Status::Doing) => cycles.push(class_id.clone()),
            None => {
                status.insert(class_id.clone(), Status::Doing);
                let node_id = &self.choices[class_id];
                let node = &egraph[node_id];
                for child in &node.children {
                    let child_cid = egraph.nid_to_cid(child);
                    self.cycle_dfs(egraph, child_cid, status, cycles)
                }
                status.insert(class_id.clone(), Status::Done);
            }
        }
    }
}

struct ClassVars {
    active: Variable,
    nodes: Vec<Variable>,
}

pub struct CbcExtractor;
impl Extractor for CbcExtractor {
    fn extract(&self, egraph: &egraph_serialize::EGraph, roots: &[ClassId]) -> ExtractionResult {
        // let mut model = Model::default();

        let mut problem_vars = ProblemVariables::new();
        let mut constraints = Vec::new();

        let true_literal = problem_vars.add(variable().binary());
        constraints.push(constraint!(true_literal == 1.0));

        let vars: IndexMap<ClassId, ClassVars> = egraph
            .classes()
            .values()
            .map(|class| {
                let cvars = ClassVars {
                    active: if roots.contains(&class.id) {
                        // Roots must be active.
                        true_literal
                    } else {
                        problem_vars.add(variable().binary())
                    },
                    nodes: class
                        .nodes
                        .iter()
                        .map(|_| problem_vars.add(variable().binary()))
                        .collect(),
                };
                (class.id.clone(), cvars)
            })
            .collect();

        for (class_id, class) in &vars {
            // class active == some node active
            // sum(for node_active in class) == class_active
            // let row = model.add_row();
            let mut row = -class.active;
            for &node_active in &class.nodes {
                row += node_active;
            }
            constraints.push(constraint!(row == 0.0));

            let childrens_classes_var = |nid: NodeId| {
                egraph[&nid]
                    .children
                    .iter()
                    .map(|n| egraph[n].eclass.clone())
                    .map(|n| vars[&n].active)
                    .collect::<IndexSet<_>>()
            };

            let mut intersection: IndexSet<_> =
                childrens_classes_var(egraph[class_id].nodes[0].clone());

            for node in &egraph[class_id].nodes[1..] {
                intersection = intersection
                    .intersection(&childrens_classes_var(node.clone()))
                    .cloned()
                    .collect();
            }

            // A class being active implies that all in the intersection
            // of it's children are too.
            for c in &intersection {
                let row = class.active - *c;
                constraints.push(constraint!(row <= 0.0));
            }

            for (node_id, &node_active) in egraph[class_id].nodes.iter().zip(&class.nodes) {
                for child_active in childrens_classes_var(node_id.clone()) {
                    // node active implies child active, encoded as:
                    //   node_active <= child_active
                    //   node_active - child_active <= 0
                    if !intersection.contains(&child_active) {
                        let row = node_active - child_active;
                        constraints.push(constraint!(row <= 0.0));
                    }
                }
            }
        }

        let mut total_cost = Expression::from(0);

        // model.set_obj_sense(Sense::Minimize);
        for class in egraph.classes().values() {
            let min_cost = class
                .nodes
                .iter()
                .map(|n_id| egraph[n_id].cost)
                .min()
                .unwrap_or(Cost::default())
                .into_inner();

            // Most helpful when the members of the class all have the same cost.
            // For example if the members' costs are [1,1,1], three terms get
            // replaced by one in the objective function.
            if min_cost != 0.0 {
                total_cost += vars[&class.id].active * min_cost;
                // model.set_obj_coeff(vars[&class.id].active, min_cost);
            }

            for (node_id, &node_active) in class.nodes.iter().zip(&vars[&class.id].nodes) {
                let node = &egraph[node_id];
                let node_cost = node.cost.into_inner() - min_cost;
                assert!(node_cost >= 0.0);

                if node_cost != 0.0 {
                    total_cost += node_active * node_cost;
                    // model.set_obj_coeff(node_active, node_cost);
                }
            }
        }

        let mut banned_cycles: IndexSet<(ClassId, usize)> = Default::default();
        find_cycles(egraph, |id, i| {
            banned_cycles.insert((id, i));
        });
        for (class_id, class_vars) in &vars {
            for (i, &node_active) in class_vars.nodes.iter().enumerate() {
                if banned_cycles.contains(&(class_id.clone(), i)) {
                    constraints.push(constraint!(node_active == 0.0));
                }
            }
        }
        log::info!("@blocked {}", banned_cycles.len());

        let problem = constraints.into_iter().fold(
            problem_vars
                .minimise(total_cost)
                .using(good_lp::default_solver),
            |acc, constraint| acc.with(constraint),
        );
        let problem = problem.set_parallel(HighsParallelType::On);

        let solution = problem.solve().unwrap();

        // let solution = model.solve();
        // log::info!(
        //     "CBC status {:?}, {:?}, obj = {}",
        //     solution.raw().status(),
        //     solution.raw().secondary_status(),
        //     solution.raw().obj_value(),
        // );

        let mut result = ExtractionResult::default();

        for (id, var) in &vars {
            let active = (solution.value(var.active) - 1.0).abs() < 0.1;
            if active {
                let node_idx = var
                    .nodes
                    .iter()
                    .position(|&n| (solution.value(n) - 1.0).abs() < 0.1)
                    .unwrap();
                let node_id = egraph[id].nodes[node_idx].clone();
                result.choose(id.clone(), node_id);
            }
        }

        let cycles = result.find_cycles(egraph, roots);
        assert!(cycles.is_empty());
        result
    }
}

// from @khaki3
// fixes bug in egg 0.9.4's version
// https://github.com/egraphs-good/egg/issues/207#issuecomment-1264737441
fn find_cycles(egraph: &egraph_serialize::EGraph, mut f: impl FnMut(ClassId, usize)) {
    let mut pending: IndexMap<ClassId, Vec<(ClassId, usize)>> = IndexMap::default();

    let mut order: IndexMap<ClassId, usize> = IndexMap::default();

    let mut memo: IndexMap<(ClassId, usize), bool> = IndexMap::default();

    let mut stack: Vec<(ClassId, usize)> = vec![];

    let n2c = |nid: &NodeId| egraph.nid_to_cid(nid);

    for class in egraph.classes().values() {
        let id = &class.id;
        for (i, node_id) in egraph[id].nodes.iter().enumerate() {
            let node = &egraph[node_id];
            for child in &node.children {
                let child = n2c(child).clone();
                pending
                    .entry(child)
                    .or_insert_with(Vec::new)
                    .push((id.clone(), i));
            }

            if node.is_leaf() {
                stack.push((id.clone(), i));
            }
        }
    }

    let mut count = 0;

    while let Some((id, i)) = stack.pop() {
        if memo.get(&(id.clone(), i)).is_some() {
            continue;
        }

        let node_id = &egraph[&id].nodes[i];
        let node = &egraph[node_id];
        let mut update = false;

        if node.is_leaf() {
            update = true;
        } else if node.children.iter().all(|x| order.get(n2c(x)).is_some()) {
            if let Some(ord) = order.get(&id) {
                update = node.children.iter().all(|x| &order[n2c(x)] < ord);
                if !update {
                    memo.insert((id, i), false);
                    continue;
                }
            } else {
                update = true;
            }
        }

        if update {
            if order.get(&id).is_none() {
                if egraph[node_id].is_leaf() {
                    order.insert(id.clone(), 0);
                } else {
                    order.insert(id.clone(), count);
                    count += 1;
                }
            }
            memo.insert((id.clone(), i), true);
            if let Some(mut v) = pending.remove(&id) {
                stack.append(&mut v);
                stack.sort();
                stack.dedup();
            };
        }
    }

    for class in egraph.classes().values() {
        let id = &class.id;
        for (i, node) in class.nodes.iter().enumerate() {
            if let Some(true) = memo.get(&(id.clone(), i)) {
                continue;
            }
            assert!(!egraph[node].is_leaf());
            f(id.clone(), i);
        }
    }
    assert!(pending.is_empty());
}

pub fn extract<L, A, C>(egraph: &EGraph<L, A>, root: Id, cost: C) -> RecExpr<L>
where
    L: Language + Display,
    A: Analysis<L>,
    C: LpCostFunction<L, A>,
{
    let root = egraph.find(root);
    let serialized_root = ClassId::from(format!("{}", root));
    let serialized_egraph = egg_to_serialized_egraph(egraph, &[serialized_root.clone()], cost);

    let extractor = CbcExtractor {};

    let result = extractor.extract(&serialized_egraph, &[serialized_root]);

    let get_by_id = |id: Id| {
        let class_id = ClassId::from(format!("{}", id));
        let node_id = result.choices.get(&class_id).unwrap();
        let node_id_str = node_id.to_string();
        let index_str = node_id_str.split('.').nth(1).unwrap();
        let index: usize = index_str.parse().unwrap();
        egraph[id].nodes[index].clone()
    };

    let expr = get_by_id(root).build_recexpr(get_by_id);

    assert!(expr.is_dag());

    expr
}

fn egg_to_serialized_egraph<L, A, C>(
    egraph: &EGraph<L, A>,
    roots: &[ClassId],
    mut cost: C,
) -> egraph_serialize::EGraph
where
    L: Language + Display,
    A: Analysis<L>,
    C: LpCostFunction<L, A>,
{
    use egraph_serialize::*;
    let mut out = EGraph::default();
    for class in egraph.classes() {
        for (i, node) in class.nodes.iter().enumerate() {
            out.add_node(
                format!("{}.{}", class.id, i),
                Node {
                    op: node.to_string(),
                    children: node
                        .children()
                        .iter()
                        .map(|id| NodeId::from(format!("{}.0", id)))
                        .collect(),
                    eclass: ClassId::from(format!("{}", class.id)),
                    cost: Cost::new(cost.node_cost(egraph, class.id, node)).unwrap(),
                },
            )
        }
    }
    out.root_eclasses.extend(roots.iter().cloned());
    out
}
