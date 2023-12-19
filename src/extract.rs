use std::{fmt::Display, collections::HashMap};

use egg::{Analysis, EGraph, Id, Language, LpCostFunction, RecExpr};
use egraph_serialize::{ClassId, NodeId, Node};
use indexmap::IndexMap;
use rustc_hash::FxHashMap;

use coin_cbc::{Col, Model, Sense};
use indexmap::IndexSet;
use ordered_float::NotNan;

// Most of this code was taken from https://github.com/egraphs-good/extraction-gym/tree/main
pub type Cost = NotNan<f64>;
pub const INFINITY: Cost = unsafe { NotNan::new_unchecked(std::f64::INFINITY) };

pub trait Extractor: Sync {
    fn extract(&self, egraph: &egraph_serialize::EGraph, roots: &[ClassId]) -> ExtractionResult;

    fn boxed(self) -> Box<dyn Extractor>
    where
        Self: Sized + 'static,
    {
        Box::new(self)
    }
}

pub trait MapGet<K, V> {
    fn get(&self, key: &K) -> Option<&V>;
}

impl<K, V> MapGet<K, V> for HashMap<K, V>
where
    K: Eq + std::hash::Hash,
{
    fn get(&self, key: &K) -> Option<&V> {
        HashMap::get(self, key)
    }
}

impl<K, V> MapGet<K, V> for FxHashMap<K, V>
where
    K: Eq + std::hash::Hash,
{
    fn get(&self, key: &K) -> Option<&V> {
        FxHashMap::get(self, key)
    }
}

impl<K, V> MapGet<K, V> for IndexMap<K, V>
where
    K: Eq + std::hash::Hash,
{
    fn get(&self, key: &K) -> Option<&V> {
        IndexMap::get(self, key)
    }
}

#[derive(Default, Clone)]
pub struct ExtractionResult {
    pub choices: IndexMap<ClassId, NodeId>,
}

#[derive(Clone, Copy)]
enum Status {
    Doing,
    Done,
}

impl ExtractionResult {
    pub fn choose(&mut self, class_id: ClassId, node_id: NodeId) {
        self.choices.insert(class_id, node_id);
    }

    pub fn find_cycles(&self, egraph: &egraph_serialize::EGraph, roots: &[ClassId]) -> Vec<ClassId> {
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

    pub fn tree_cost(&self, egraph: &egraph_serialize::EGraph, roots: &[ClassId]) -> Cost {
        let node_roots = roots
            .iter()
            .map(|cid| self.choices[cid].clone())
            .collect::<Vec<NodeId>>();
        self.tree_cost_rec(egraph, &node_roots, &mut HashMap::new())
    }

    fn tree_cost_rec(
        &self,
        egraph: &egraph_serialize::EGraph,
        roots: &[NodeId],
        memo: &mut HashMap<NodeId, Cost>,
    ) -> Cost {
        let mut cost = Cost::default();
        for root in roots {
            if let Some(c) = memo.get(root) {
                cost += *c;
                continue;
            }
            let class = egraph.nid_to_cid(root);
            let node = &egraph[&self.choices[class]];
            let inner = node.cost + self.tree_cost_rec(egraph, &node.children, memo);
            memo.insert(root.clone(), inner);
            cost += inner;
        }
        cost
    }

    // this will loop if there are cycles
    pub fn dag_cost(&self, egraph: &egraph_serialize::EGraph, roots: &[ClassId]) -> Cost {
        let mut costs: IndexMap<ClassId, Cost> = IndexMap::new();
        let mut todo: Vec<ClassId> = roots.to_vec();
        while let Some(cid) = todo.pop() {
            let node_id = &self.choices[&cid];
            let node = &egraph[node_id];
            if costs.insert(cid.clone(), node.cost).is_some() {
                continue;
            }
            for child in &node.children {
                todo.push(egraph.nid_to_cid(child).clone());
            }
        }
        costs.values().sum()
    }

    pub fn node_sum_cost<M>(&self, egraph: &egraph_serialize::EGraph, node: &Node, costs: &M) -> Cost
    where
        M: MapGet<ClassId, Cost>,
    {
        node.cost
            + node
                .children
                .iter()
                .map(|n| {
                    let cid = egraph.nid_to_cid(n);
                    costs.get(cid).unwrap_or(&INFINITY)
                })
                .sum::<Cost>()
    }
}

struct ClassVars {
    active: Col,
    nodes: Vec<Col>,
}

pub struct CbcExtractor;
impl Extractor for CbcExtractor {
    fn extract(&self, egraph: &egraph_serialize::EGraph, roots: &[ClassId]) -> ExtractionResult {
        let mut model = Model::default();

        let true_literal = model.add_binary();
        model.set_col_lower(true_literal, 1.0);

        let vars: IndexMap<ClassId, ClassVars> = egraph
            .classes()
            .values()
            .map(|class| {
                let cvars = ClassVars {
                    active: if roots.contains(&class.id) {
                        // Roots must be active.
                        true_literal
                    } else {
                        model.add_binary()
                    },
                    nodes: class.nodes.iter().map(|_| model.add_binary()).collect(),
                };
                (class.id.clone(), cvars)
            })
            .collect();

        for (class_id, class) in &vars {
            // class active == some node active
            // sum(for node_active in class) == class_active
            let row = model.add_row();
            model.set_row_equal(row, 0.0);
            model.set_weight(row, class.active, -1.0);
            for &node_active in &class.nodes {
                model.set_weight(row, node_active, 1.0);
            }

            let childrens_classes_var = |nid: NodeId| {
                egraph[&nid]
                    .children
                    .iter()
                    .map(|n| egraph[n].eclass.clone())
                    .map(|n| vars[&n].active)
                    .collect::<IndexSet<_>>()
            };

            let mut intersection: IndexSet<Col> =
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
                let row = model.add_row();
                model.set_row_upper(row, 0.0);
                model.set_weight(row, class.active, 1.0);
                model.set_weight(row, *c, -1.0);
            }

            for (node_id, &node_active) in egraph[class_id].nodes.iter().zip(&class.nodes) {
                for child_active in childrens_classes_var(node_id.clone()) {
                    // node active implies child active, encoded as:
                    //   node_active <= child_active
                    //   node_active - child_active <= 0
                    if !intersection.contains(&child_active) {
                        let row = model.add_row();
                        model.set_row_upper(row, 0.0);
                        model.set_weight(row, node_active, 1.0);
                        model.set_weight(row, child_active, -1.0);
                    }
                }
            }
        }

        model.set_obj_sense(Sense::Minimize);
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
                model.set_obj_coeff(vars[&class.id].active, min_cost);
            }

            for (node_id, &node_active) in class.nodes.iter().zip(&vars[&class.id].nodes) {
                let node = &egraph[node_id];
                let node_cost = node.cost.into_inner() - min_cost;
                assert!(node_cost >= 0.0);

                if node_cost != 0.0 {
                    model.set_obj_coeff(node_active, node_cost);
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
                    model.set_col_upper(node_active, 0.0);
                    model.set_col_lower(node_active, 0.0);
                }
            }
        }
        log::info!("@blocked {}", banned_cycles.len());

        let solution = model.solve();
        log::info!(
            "CBC status {:?}, {:?}, obj = {}",
            solution.raw().status(),
            solution.raw().secondary_status(),
            solution.raw().obj_value(),
        );

        let mut result = ExtractionResult::default();

        for (id, var) in &vars {
            let active = solution.col(var.active) > 0.0;
            if active {
                let node_idx = var
                    .nodes
                    .iter()
                    .position(|&n| solution.col(n) > 0.0)
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
