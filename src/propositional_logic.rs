use std::{fmt, str::FromStr, marker::PhantomData};

use crate::formula::*;
use crate::opperators::*;

#[derive(Debug)]
pub struct PropositionalLanguage<A: fmt::Display + FromStr> {
    _phantom_data: PhantomData<A>
}

group_opps!(PLBinOpps, And, Or, Implie, IfAndOnlyIf);
group_opps!(PLFuncs, Conjuction, Disjuction);

impl<A: fmt::Display + FromStr> Language for PropositionalLanguage<A> {
    type Atom = A;
    type UnaryOpp = Neg;
    type BinaryOpp = PLBinOpps;
    type Function = PLFuncs;
}


pub mod boolean_interpretation {
    use std::fmt;
    use std::str::FromStr;
    use std::ops::{Not, BitAnd, BitOr};

    use super::super::interpretation::*;
    use super::PropositionalLanguage;

    pub struct BooleanInterpretation;

    impl<
        Type: Copy + Not<Output = Type> + BitAnd<Output = Type> + BitOr<Output = Type>,
        Atom: fmt::Display + FromStr
    > Interpretation<PropositionalLanguage<Atom>, Type> for BooleanInterpretation {
        fn unary(_o: &<PropositionalLanguage<Atom> as crate::formula::Language>::UnaryOpp, a: Type) -> Type {
            !a
        }
        fn binary(a: Type, o: &<PropositionalLanguage<Atom> as crate::formula::Language>::BinaryOpp, b: Type) -> Type {
            match o {
                super::PLBinOpps::And(_) => a & b,
                super::PLBinOpps::Or(_) => a | b,
                super::PLBinOpps::Implie(_) => !a | b,
                super::PLBinOpps::IfAndOnlyIf(_) => (a & b) | (!a & !b),
            }
        }
        fn function<I: Iterator<Item = Type>>(f: &<PropositionalLanguage<Atom> as crate::formula::Language>::Function, args: I) -> Type {
            match f {
                super::PLFuncs::Conjuction(_) => args.reduce(BitAnd::bitand).unwrap(),
                super::PLFuncs::Disjuction(_) => args.reduce(BitOr::bitor).unwrap(),
            }
        }
    }
}

