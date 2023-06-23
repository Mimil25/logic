use std::collections::{HashMap};
use std::fmt::Debug;

use crate::formula::{Language, Atom, Formula};

pub trait Interpretation<L: Language, Type> {
    fn unary(o: &L::UnaryOpp, a: Type) -> Type;
    fn binary(a: Type, o: &L::BinaryOpp, b: Type) -> Type;
    fn function<I: Iterator<Item = Type>>(f: &L::Function, args: I) -> Type;
}

pub trait Valuation<A: Atom, Type> {
    fn valuation(&self, atom: &A) -> Type;
}

#[derive(Debug)]
pub struct HashMapValuation<A: Atom, Type: Debug> {
    map: HashMap<A, usize>,
    pub data: Vec<Type>
}

impl<A: Atom, Type: Copy + Debug> Valuation<A, Type> for HashMapValuation<A, Type> {
    fn valuation(&self, atom: &A) -> Type {
        self.data[*self.map.get(atom).unwrap()]
    }
}

impl<A: Atom> IntoIterator for HashMapValuation<A, bool> {
    type IntoIter = HashMapValuationIter<A>;
    type Item = <Self::IntoIter as Iterator>::Item;
    fn into_iter(mut self) -> Self::IntoIter { 
        for v in self.data.iter_mut() {
            *v = false;
        }
        HashMapValuationIter::<A> {
            values: self,
        }
    }
}

pub struct HashMapValuationIter<A: Atom> {
    pub values: HashMapValuation<A, bool>,
}

impl<A: Atom> Iterator for HashMapValuationIter<A> {
    type Item = ();
    // set the values to their next position
    fn next(&mut self) -> Option<Self::Item> {
        let mut carry = true;
        for v in self.values.data.iter_mut() {
            let t = *v;
            *v ^= carry;
            carry &= t;
        }
        if carry{
            None
        } else {
            Some(())
        }
    }
}

impl<I: Iterator> From<I> for HashMapValuationIter<I::Item> where I::Item: Atom{
    fn from(value: I) -> Self {
        let vals: Vec<I::Item> = value.collect();
        let map: HashMap<I::Item, usize> = vals.iter()
            .enumerate()
            .filter(|(i, a)| !vals[0..*i].contains(a))
            .map(|(_, a)| a)
            .enumerate()
            .map(|(i, a)| (a.to_owned(), i))
            .collect();
        let n = map.len();
        Self {
            values: HashMapValuation {
                map,
                data: Vec::from_iter((0..n).map(|_| false)),
            }
        }
    }
}

pub fn evaluate<L: Language,
            Type, 
            I: Interpretation<L, Type>,
            V: Valuation<L::Atom, Type>>
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
