use std::collections::HashMap;

use crate::formula::{Language, Formula};

pub trait Interpretation<L: Language, Type> {
    fn unary(o: &L::UnaryOpp, a: Type) -> Type;
    fn binary(a: Type, o: &L::BinaryOpp, b: Type) -> Type;
    fn function<I: Iterator<Item = Type>>(f: &L::Function, args: I) -> Type;
}

pub trait Valuation<L: Language, Type> {
    fn valuation(&self, atom: &L::Atom) -> Type;
}

#[derive(Debug)]
pub struct HashMapValuation<L: Language, Type> where L::Atom: Eq + std::hash::Hash {
    map: HashMap<L::Atom, Type>,
}

impl<L: Language, Type: Copy> Valuation<L, Type> for HashMapValuation<L, Type> where L::Atom: Eq + std::hash::Hash {
    fn valuation(&self, atom: &<L as Language>::Atom) -> Type {
        *self.map.get(atom).unwrap()
    }
}

impl<L: Language, Type> TryFrom<HashMap<&str, Type>> for HashMapValuation<L, Type> where L::Atom: Eq + std::hash::Hash {
    type Error = <L::Atom as std::str::FromStr>::Err;
    fn try_from(value: HashMap<&str, Type>) -> Result<Self, Self::Error> {
        let mut map = Vec::with_capacity(value.len());
        for (k, v) in value.into_iter() {
            map.push((k.parse()?, v));
        }
        Ok(Self { map: map.into_iter().collect() })
    }
}

pub fn evaluate<L: Language,
            Type, 
            I: Interpretation<L, Type>,
            V: Valuation<L, Type>>
            (
                f: &Formula<L>,
                v: &V
) -> Type {
    match f {
        Formula::Atom(atom) => v.valuation(atom),
        Formula::UnaryOpp(o, a) => I::unary(
            o,
            evaluate::<L, Type, I, V>(a, v)
        ),
        Formula::BinaryOpp(a, o, b) => I::binary(
            evaluate::<L, Type, I, V>(a, v),
            o,
            evaluate::<L, Type, I, V>(b, v)
        ),
        Formula::Function(f, args) => I::function(
            f,
            args.iter().map(|arg| {
                evaluate::<L, Type, I, V>(arg, v)
            })
        ),
    }
}
