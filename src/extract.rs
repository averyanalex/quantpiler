use std::hash::BuildHasherDefault;

use egg::*;
use rustc_hash::FxHashMap;

// Most of this code was taken from https://github.com/egraphs-good/extraction-gym

#[derive(Default, Clone, Debug)]
struct ExtractionResult {
    choices: FxHashMap<Id, usize>,
}

impl ExtractionResult {
    fn set_choose(&mut self, class: Id, node: usize) {
        self.choices.insert(class, node);
    }

    fn get_choose(&self, class: &Id) -> usize {
        self.choices[class]
    }
}

pub trait LpCostFunction<L: Language, N: Analysis<L>> {
    fn node_cost(&mut self, egraph: &EGraph<L, N>, eclass: Id, enode: &L) -> f64;
}

struct CostSet {
    costs: FxHashMap<Id, f64>,
    total: f64,
    choice: usize,
}

pub fn extract<L: Language, N: Analysis<L>, C: LpCostFunction<L, N>>(
    egraph: &EGraph<L, N>,
    root: Id,
    cost: &mut C,
) -> RecExpr<L> {
    let mut costs = FxHashMap::<Id, CostSet>::with_capacity_and_hasher(
        egraph.classes().len(),
        BuildHasherDefault::default(),
    );

    let mut keep_going = true;

    while keep_going {
        keep_going = false;

        'node_loop: for (cid, nid, node) in egraph.classes().flat_map(|eclass| {
            eclass
                .nodes
                .iter()
                .enumerate()
                .map(|(nid, node)| (egraph.find(eclass.id), nid, node))
        }) {
            let mut cost_set = CostSet {
                costs: FxHashMap::default(),
                total: f64::default(),
                choice: nid,
            };

            // compute the cost set from the children
            for child in node.children().iter().map(|c| egraph.find(*c)) {
                if let Some(child_cost_set) = costs.get(&child) {
                    // prevent a cycle
                    if child_cost_set.costs.contains_key(&cid) {
                        continue 'node_loop;
                    }
                    cost_set.costs.extend(child_cost_set.costs.clone());
                } else {
                    continue 'node_loop;
                }
            }

            // add this node
            cost_set
                .costs
                .insert(cid, cost.node_cost(egraph, cid, node));

            cost_set.total = cost_set.costs.values().sum();

            // if the cost set is better than the current one, update it
            if let Some(old_cost_set) = costs.get(&cid) {
                if cost_set.total < old_cost_set.total {
                    costs.insert(cid, cost_set);
                    keep_going = true;
                }
            } else {
                costs.insert(cid, cost_set);
                keep_going = true;
            }
        }
    }

    let mut result = ExtractionResult::default();
    for (cid, cost_set) in costs {
        result.set_choose(cid, cost_set.choice);
    }

    let get_by_id = |id: Id| {
        let id = egraph.find(id);
        let eclass = &egraph[id];
        eclass.nodes[result.get_choose(&id)].clone()
    };

    get_by_id(root).build_recexpr(get_by_id)
}
