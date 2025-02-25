use std::{fmt::Display, str::FromStr};

use egg::*;
use num::{traits::Pow, BigUint, ToPrimitive, Zero};

use crate::extract::LpCostFunction;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ArgumentInfo {
    pub size: usize,
    pub name: String,
}

impl Display for ArgumentInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({})", self.name, self.size)
    }
}

impl FromStr for ArgumentInfo {
    type Err = ();

    fn from_str(_s: &str) -> Result<Self, Self::Err> {
        Err(())
    }
}

define_language! {
    pub enum Op {
        "!" = Not(Id),
        "^" = Xor([Id; 2]),
        "|" = Or([Id; 2]),
        "&" = And([Id; 2]),
        ">>" = Shr([Id; 2]),
        "<<" = Shl([Id; 2]),
        "+" = Add([Id; 2]),
        "+%" = AddRem([Id; 3]),
        "-" = Sub([Id; 2]),
        "*" = Mul([Id; 2]),
        "*%" = MulRem([Id; 3]),
        "//" = Div([Id; 2]),
        "%" = Rem([Id; 2]),
        "**" = Pow([Id; 2]),
        "**%" = PowRem([Id; 3]),
        "==" = Eq([Id; 2]),
        "<" = Lt([Id; 2]),
        ">" = Gt([Id; 2]),
        "!=" = Ne([Id; 2]),
        ">=" = Ge([Id; 2]),
        "<=" = Le([Id; 2]),
        "?" = Ternary([Id; 3]),
        Constant(BigUint),
        Argument(ArgumentInfo),
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct AnalyzerData {
    value: Option<BigUint>,
    // length: u32,
}

#[allow(clippy::cognitive_complexity)]
pub fn eval_enode(
    enode: &Op,
    val: impl Fn(Id) -> Option<BigUint>,
    arg: impl Fn(&'_ ArgumentInfo) -> Option<BigUint>,
) -> Option<BigUint> {
    Some(match *enode {
        Op::Not(a) => BigUint::new(val(a)?.iter_u32_digits().map(|d| !d).collect()),
        Op::Xor([a, b]) => val(a)? ^ val(b)?,
        Op::Or([a, b]) => val(a)? | val(b)?,
        Op::And([a, b]) => val(a)? & val(b)?,
        Op::Shr([t, d]) => val(t)? >> val(d)?.to_u128()?,
        Op::Shl([t, d]) => val(t)? << val(d)?.to_u128()?,
        Op::Add([a, b]) => val(a)? + val(b)?,
        Op::AddRem([a, b, c]) => {
            let c = val(c)?;
            ((val(a)? % &c) + (val(b)? % &c)) % &c
        }
        Op::Sub([a, b]) => val(a)? - val(b)?,
        Op::Mul([a, b]) => val(a)? * val(b)?,
        Op::MulRem([a, b, c]) => {
            let c = val(c)?;
            ((val(a)? % &c) * (val(b)? % &c)) % &c
        }
        Op::Div([a, b]) => val(a)? / val(b)?,
        Op::Rem([a, b]) => val(a)? % val(b)?,
        Op::Pow([a, b]) => Pow::pow(val(a)?, val(b)?.to_u128()?),
        Op::PowRem([a, b, c]) => val(a)?.modpow(&val(b)?, &val(c)?),
        Op::Eq([a, b]) => BigUint::from(val(a)? == val(b)?),
        Op::Lt([a, b]) => BigUint::from(val(a)? < val(b)?),
        Op::Gt([a, b]) => BigUint::from(val(a)? > val(b)?),
        Op::Ne([a, b]) => BigUint::from(val(a)? != val(b)?),
        Op::Ge([a, b]) => BigUint::from(val(a)? >= val(b)?),
        Op::Le([a, b]) => BigUint::from(val(a)? <= val(b)?),
        Op::Ternary([cond, then, or]) => {
            if val(cond)?.is_zero() {
                val(or)?
            } else {
                val(then)?
            }
        }
        Op::Constant(ref c) => c.clone(),
        Op::Argument(ref a) => arg(a)?,
    })
}

#[derive(Default, Clone)]
pub struct Analyzer;
impl Analysis<Op> for Analyzer {
    type Data = AnalyzerData;

    fn merge(&mut self, to: &mut Self::Data, from: Self::Data) -> DidMerge {
        egg::merge_max(to, from)
    }

    fn make(egraph: &EGraph<Op, Self>, enode: &Op) -> Self::Data {
        let value = eval_enode(enode, |i| egraph[i].data.value.clone(), |_| None);
        Self::Data { value }
    }

    fn modify(egraph: &mut EGraph<Op, Self>, id: Id) {
        if let Some(i) = &egraph[id].data.value {
            let added = egraph.add(Op::Constant(i.clone()));
            egraph.union(id, added);
        }
    }
}

pub fn make_rules() -> Vec<Rewrite<Op, Analyzer>> {
    use egg::rewrite as rw;
    let mut rules = vec![
        // Commutable
        rw!("comm-xor"; "(^ ?a ?b)" => "(^ ?b ?a)"),
        rw!("comm-or"; "(| ?a ?b)" => "(| ?b ?a)"),
        rw!("comm-and"; "(& ?a ?b)" => "(& ?b ?a)"),
        rw!("comm-add"; "(+ ?a ?b)" => "(+ ?b ?a)"),
        rw!("comm-add-rem"; "(+% ?a ?b ?c)" => "(+% ?b ?a ?c)"),
        rw!("comm-mul"; "(* ?a ?b)" => "(* ?b ?a)"),
        rw!("comm-mul-rem"; "(*% ?a ?b ?c)" => "(*% ?b ?a ?c)"),
        rw!("comm-eq"; "(== ?a ?b)" => "(== ?b ?a)"),
        // Associative
        rw!("assoc-xor"; "(^ ?a (^ ?b ?c))" => "(^ (^ ?a ?b) ?c)"),
        rw!("assoc-or"; "(| ?a (| ?b ?c))" => "(| (| ?a ?b) ?c)"),
        rw!("assoc-and"; "(& ?a (& ?b ?c))" => "(& (& ?a ?b) ?c)"),
        rw!("assoc-add"; "(+ ?a (+ ?b ?c))" => "(+ (+ ?a ?b) ?c)"),
        rw!("assoc-mul"; "(* ?a (* ?b ?c))" => "(* (* ?a ?b) ?c)"),
        rw!("assoc-sub"; "(- ?a (- ?b ?c))" => "(+ (- ?a ?b) ?c)"),
        // Same elements logic
        rw!("xor-same"; "(^ ?a ?a)" => "0"),
        rw!("sub-same"; "(- ?a ?a)" => "0"),
        // rw!("xor-same-not"; "(^ (! ?a) ?a)" => "1111"),
        rw!("or-same"; "(| ?a ?a)" => "?a"),
        // rw!("or-same-not"; "(| (! ?a) ?a)" => "1111"),
        rw!("and-same"; "(& ?a ?a)" => "?a"),
        rw!("and-same-not"; "(& (! ?a) ?a)" => "0"),
        // With constants
        rw!("and-with-0"; "(& ?a 0)" => "0"),
        rw!("or-with-0"; "(| ?a 0)" => "?a"),
        rw!("xor-with-0"; "(^ ?a 0)" => "?a"),
        rw!("add-with-0"; "(+ ?a 0)" => "?a"),
        rw!("sub-with-0"; "(- ?a 0)" => "?a"),
        rw!("mul-with-0"; "(* ?a 0)" => "0"),
        rw!("mul-with-1"; "(* ?a 1)" => "?a"),
        // Etc
        rw!("a-mul2-a-add-a"; "(* ?a 2)" => "(+ ?a ?a)"),
        rw!("a-add-a-a-mul2"; "(+ ?a ?a)" => "(* ?a 2)"),
        rw!("cancel-not"; "(! (! ?a))" => "?a"),
        rw!("xor-not-not-xor"; "(^ (! ?a) ?b)" => "(! (^ ?a ?b))"),
        rw!("cancel-xor-not-not"; "(^ (! ?a) (! ?b))" => "(^ ?a ?b)"),
        rw!("not-xor-xor-not"; "(! (^ ?a ?b))" => "(^ (! ?a) ?b)"),
        rw!("create-mul-one"; "?a" => "(* ?a 1)"),
        rw!("merge-add-muls"; "(+ ?a (* ?a ?b))" => "(* ?a (+ 1 ?b))"),
        // Mod(Op(_, _), _) => OpMod(_, _, _)
        rw!("add-rem"; "(% (+ ?a ?b) ?c)" => "(+% ?a ?b ?c)"),
        rw!("mul-rem"; "(% (* ?a ?b) ?c)" => "(*% ?a ?b ?c)"),
        rw!("pow-rem"; "(% (** ?a ?b) ?c)" => "(**% ?a ?b ?c)"),
    ];

    // Distributivity
    rules.append(
        &mut [rw!("distr-mul-add"; "(* (+ ?a ?b) ?c)" <=> "(+ (* ?a ?c) (* ?b ?c))")].concat(),
    );

    rules
}

pub struct Cost;

impl LpCostFunction<Op, Analyzer> for Cost {
    fn node_cost(&mut self, _egraph: &EGraph<Op, Analyzer>, _id: Id, enode: &Op) -> f64 {
        // let l = f64::from(egraph[id].data.length);
        let l = 1.0;

        #[allow(clippy::match_same_arms)]
        match enode {
            Op::Not(_) | Op::Shr(_) | Op::Shl(_) => 1.0 * l,
            Op::Xor(_) | Op::And(_) => 2.0 * l,
            Op::Or(_) => 5.0 * l, // a | b = !(!a & !b)
            Op::Eq(_) | Op::Ne(_) | Op::Ternary(_) => 4.0 * l,
            Op::Add(_) | Op::Sub(_) | Op::Gt(_) | Op::Lt(_) | Op::Le(_) | Op::Ge(_) => 32.0 * l,
            Op::AddRem(_) => 64.0 * l,
            Op::Mul(_) => 64.0 * l,
            Op::MulRem(_) => (64.0 + 32.0) * l,
            Op::Div(_) => todo!(),
            Op::Rem(_) => 128.0 * l,
            Op::Pow(_) => 128.0 * l,
            Op::PowRem(_) => (128.0 + 32.0) * l,
            Op::Constant(_) => 0.1 * l,
            Op::Argument(_) => 0.1 * l,
        }
    }
}
