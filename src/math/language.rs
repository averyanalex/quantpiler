use std::{cmp::Ordering, fmt::Display, str::FromStr};

use egg::{Analysis, DidMerge, EGraph, Id, Rewrite, define_language};
use num::{BigUint, ToPrimitive, Zero, traits::Pow};

use crate::extract::LpCostFunction;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ArgumentInfo {
    pub length: u32,
    pub name: String,
}

impl Display for ArgumentInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.name, self.length)
    }
}

impl FromStr for ArgumentInfo {
    type Err = ();

    fn from_str(_s: &str) -> Result<Self, Self::Err> {
        Err(())
    }
}

define_language! {
    pub enum MathLanguage {
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
        // If, then, else
        "?" = Ternary([Id; 3]),
        Constant(BigUint),
        Argument(ArgumentInfo),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnalyzerData {
    value: Option<BigUint>,
    max_length: u32,
}

pub fn eval_enode<'a>(
    enode: &MathLanguage,
    val: impl Fn(Id) -> Option<&'a BigUint>,
) -> Option<BigUint> {
    Some(match *enode {
        MathLanguage::Not(a) => BigUint::new(val(a)?.iter_u32_digits().map(|d| !d).collect()),
        MathLanguage::Xor([a, b]) => val(a)? ^ val(b)?,
        MathLanguage::Or([a, b]) => val(a)? | val(b)?,
        MathLanguage::And([a, b]) => val(a)? & val(b)?,
        MathLanguage::Shr([t, d]) => val(t)? >> val(d)?.to_u32()?,
        MathLanguage::Shl([t, d]) => val(t)? << val(d)?.to_u32()?,
        MathLanguage::Add([a, b]) => val(a)? + val(b)?,
        MathLanguage::AddRem([a, b, c]) => (val(a)? + val(b)?) % val(c)?,
        MathLanguage::Sub([a, b]) => val(a)? - val(b)?,
        MathLanguage::Mul([a, b]) => val(a)? * val(b)?,
        MathLanguage::MulRem([a, b, c]) => (val(a)? * val(b)?) % val(c)?,
        MathLanguage::Div([a, b]) => val(a)? / val(b)?,
        MathLanguage::Rem([a, b]) => val(a)? % val(b)?,
        MathLanguage::Pow([a, b]) => val(a)?.pow(val(b)?.to_u32()?),
        MathLanguage::PowRem([a, b, c]) => val(a)?.modpow(val(b)?, val(c)?),
        MathLanguage::Eq([a, b]) => BigUint::from(val(a)? == val(b)?),
        MathLanguage::Lt([a, b]) => BigUint::from(val(a)? < val(b)?),
        MathLanguage::Gt([a, b]) => BigUint::from(val(a)? > val(b)?),
        MathLanguage::Ne([a, b]) => BigUint::from(val(a)? != val(b)?),
        MathLanguage::Ge([a, b]) => BigUint::from(val(a)? >= val(b)?),
        MathLanguage::Le([a, b]) => BigUint::from(val(a)? <= val(b)?),
        MathLanguage::Ternary([cond, then, or]) => {
            if val(cond)?.is_zero() {
                val(or)?.clone()
            } else {
                val(then)?.clone()
            }
        }
        MathLanguage::Constant(ref c) => c.clone(),
        MathLanguage::Argument(_) => return None,
    })
}

#[derive(Default, Clone)]
pub struct MathAnalyzer;
impl Analysis<MathLanguage> for MathAnalyzer {
    type Data = AnalyzerData;

    fn merge(&mut self, to: &mut Self::Data, from: Self::Data) -> DidMerge {
        let mut merge_result = Self::Data {
            value: from.value.clone(),
            max_length: to.max_length.min(from.max_length),
        };

        if let Some(to_value) = &to.value {
            if let Some(from_value) = &merge_result.value {
                debug_assert_eq!(to_value, from_value);
            } else {
                merge_result.value = Some(to_value.clone());
            }
        }

        let did_merge = DidMerge(*to != merge_result, from != merge_result);
        *to = merge_result;
        did_merge
    }

    fn make(
        egraph: &mut egg::EGraph<MathLanguage, MathAnalyzer>,
        enode: &MathLanguage,
    ) -> Self::Data {
        let value = eval_enode(enode, |i| egraph[i].data.value.as_ref());
        Self::Data {
            value,
            max_length: 1, // TODO: calculate real length
        }
    }

    fn modify(egraph: &mut EGraph<MathLanguage, Self>, id: Id) {
        if let Some(constant) = &egraph[id].data.value {
            let added = egraph.add(MathLanguage::Constant(constant.clone()));
            egraph.union(id, added);
        }
    }
}

pub fn make_rules() -> Vec<Rewrite<MathLanguage, MathAnalyzer>> {
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
        rw!("comm-ne"; "(!= ?a ?b)" => "(!= ?b ?a)"),
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
        rw!("or-same"; "(| ?a ?a)" => "?a"),
        rw!("and-same"; "(& ?a ?a)" => "?a"),
        rw!("and-not-same"; "(& (! ?a) ?a)" => "0"),
        
        // Shift operations with constants
        rw!("shl-with-0"; "(<< ?a 0)" => "?a"),
        rw!("shr-with-0"; "(>> ?a 0)" => "?a"),
        rw!("shl-0"; "(<< 0 ?n)" => "0"),
        rw!("shr-0"; "(>> 0 ?n)" => "0"),
        
        // Power operations with constants
        rw!("pow-with-0"; "(** ?a 0)" => "1"),
        rw!("pow-with-1"; "(** ?a 1)" => "?a"),
        rw!("pow-0"; "(** 0 ?n)" => "0"),
        rw!("pow-1"; "(** 1 ?n)" => "1"),
        
        // Division operations with constants  
        rw!("div-with-1"; "(// ?a 1)" => "?a"),
        rw!("div-0"; "(// 0 ?n)" => "0"),
        
        // Remainder operations with constants
        rw!("rem-with-1"; "(% ?a 1)" => "0"),
        rw!("rem-0"; "(% 0 ?n)" => "0"),
        
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
        // Ternary rules
        // Basic simplifications
        rw!("ternary-false"; "(? 0 ?then ?else)" => "?else"),
        rw!("ternary-same"; "(? ?cond ?same ?same)" => "?same"),
        // Boolean conversion
        rw!("ternary-to-ne"; "(? ?cond 1 0)" => "(!= ?cond 0)"),
        rw!("ternary-to-eq"; "(? ?cond 0 1)" => "(== ?cond 0)"),
        // Nested ternary simplifications
        rw!("ternary-nested-cond"; "(? ?cond (? ?cond ?a ?b) ?c)" => "(? ?cond ?a ?c)"),
        rw!("ternary-nested-else"; "(? ?cond ?a (? ?cond ?b ?c))" => "(? ?cond ?a ?c)"),
        // Ternary distributivity over operations
        rw!("ternary-distr-add"; "(+ (? ?cond ?a ?b) ?c)" => "(? ?cond (+ ?a ?c) (+ ?b ?c))"),
        rw!("ternary-distr-sub"; "(- (? ?cond ?a ?b) ?c)" => "(? ?cond (- ?a ?c) (- ?b ?c))"),
        rw!("ternary-distr-mul"; "(* (? ?cond ?a ?b) ?c)" => "(? ?cond (* ?a ?c) (* ?b ?c))"),
        rw!("ternary-distr-xor"; "(^ (? ?cond ?a ?b) ?c)" => "(? ?cond (^ ?a ?c) (^ ?b ?c))"),
        rw!("ternary-distr-and"; "(& (? ?cond ?a ?b) ?c)" => "(? ?cond (& ?a ?c) (& ?b ?c))"),
        rw!("ternary-distr-or"; "(| (? ?cond ?a ?b) ?c)" => "(? ?cond (| ?a ?c) (| ?b ?c))"),
        // Reverse distributivity (right operand)
        rw!("ternary-distr-add-r"; "(+ ?c (? ?cond ?a ?b))" => "(? ?cond (+ ?c ?a) (+ ?c ?b))"),
        rw!("ternary-distr-mul-r"; "(* ?c (? ?cond ?a ?b))" => "(? ?cond (* ?c ?a) (* ?c ?b))"),
        rw!("ternary-distr-xor-r"; "(^ ?c (? ?cond ?a ?b))" => "(? ?cond (^ ?c ?a) (^ ?c ?b))"),
        rw!("ternary-distr-and-r"; "(& ?c (? ?cond ?a ?b))" => "(? ?cond (& ?c ?a) (& ?c ?b))"),
        rw!("ternary-distr-or-r"; "(| ?c (? ?cond ?a ?b))" => "(? ?cond (| ?c ?a) (| ?c ?b))"),
        // Ternary with comparisons
        rw!("ternary-distr-eq"; "(== (? ?cond ?a ?b) ?c)" => "(? ?cond (== ?a ?c) (== ?b ?c))"),
        rw!("ternary-distr-ne"; "(!= (? ?cond ?a ?b) ?c)" => "(? ?cond (!= ?a ?c) (!= ?b ?c))"),
        rw!("ternary-distr-lt"; "(< (? ?cond ?a ?b) ?c)" => "(? ?cond (< ?a ?c) (< ?b ?c))"),
        rw!("ternary-distr-gt"; "(> (? ?cond ?a ?b) ?c)" => "(? ?cond (> ?a ?c) (> ?b ?c))"),
        rw!("ternary-distr-le"; "(<= (? ?cond ?a ?b) ?c)" => "(? ?cond (<= ?a ?c) (<= ?b ?c))"),
        rw!("ternary-distr-ge"; "(>= (? ?cond ?a ?b) ?c)" => "(? ?cond (>= ?a ?c) (>= ?b ?c))"),
        // Ternary condition inversions
        rw!("ternary-invert-cond"; "(? (! ?cond) ?then ?else)" => "(? ?cond ?else ?then)"),
        rw!("ternary-eq-zero"; "(? (== ?cond 0) ?then ?else)" => "(? ?cond ?else ?then)"),
        rw!("ternary-ne-zero"; "(? (!= ?cond 0) ?then ?else)" => "(? ?cond ?then ?else)"),
        
        // Additional ternary simplifications
        rw!("ternary-not"; "(! (? ?cond ?then ?else))" => "(? ?cond (! ?then) (! ?else))"),
        rw!("ternary-with-zero-then"; "(? ?cond 0 ?else)" => "(& (== ?cond 0) ?else)"),
        rw!("ternary-with-zero-else"; "(? ?cond ?then 0)" => "(& (!= ?cond 0) ?then)"),
        
        // Ternary optimization patterns
        rw!("ternary-double-neg"; "(? (! (! ?cond)) ?then ?else)" => "(? ?cond ?then ?else)"),
        rw!("ternary-comp-invert"; "(? (< ?a ?b) ?then ?else)" => "(? (>= ?a ?b) ?else ?then)"),
        rw!("ternary-comp-invert-eq"; "(? (== ?a ?b) ?then ?else)" => "(? (!= ?a ?b) ?else ?then)"),
    ];

    // Distributivity
    rules.append(
        &mut [rw!("distr-mul-add"; "(* (+ ?a ?b) ?c)" <=> "(+ (* ?a ?c) (* ?b ?c))")].concat(),
    );

    rules
}

pub struct MathCost;

impl LpCostFunction<MathLanguage, MathAnalyzer> for MathCost {
    fn node_cost(
        &mut self,
        _egraph: &EGraph<MathLanguage, MathAnalyzer>,
        _id: Id,
        enode: &MathLanguage,
    ) -> f64 {
        // let l = f64::from(egraph[id].data.length);
        let l = 1.0;

        #[allow(clippy::match_same_arms)]
        match enode {
            MathLanguage::Not(_) | MathLanguage::Shr(_) | MathLanguage::Shl(_) => 1.0 * l,
            MathLanguage::Xor(_) | MathLanguage::And(_) => 2.0 * l,
            MathLanguage::Or(_) => 5.0 * l, // a | b = !(!a & !b)
            MathLanguage::Eq(_) | MathLanguage::Ne(_) | MathLanguage::Ternary(_) => 4.0 * l,
            MathLanguage::Add(_)
            | MathLanguage::Sub(_)
            | MathLanguage::Gt(_)
            | MathLanguage::Lt(_)
            | MathLanguage::Le(_)
            | MathLanguage::Ge(_) => 32.0 * l,
            MathLanguage::AddRem(_) => 64.0 * l,
            MathLanguage::Mul(_) => 64.0 * l,
            MathLanguage::MulRem(_) => (64.0 + 32.0) * l,
            MathLanguage::Div(_) => todo!(),
            MathLanguage::Rem(_) => 128.0 * l,
            MathLanguage::Pow(_) => 128.0 * l,
            MathLanguage::PowRem(_) => (128.0 + 32.0) * l,
            MathLanguage::Constant(_) => 0.1 * l,
            MathLanguage::Argument(_) => 0.1 * l,
        }
    }
}
