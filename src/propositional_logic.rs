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
    use std::{fmt, io};
    use std::str::FromStr;
    use std::ops::{Not, BitAnd, BitOr};
    use std::collections::HashSet;

    use crate::formula::{Formula, Language, get_atoms};

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

    pub fn truth_table_totology
        <Atom: FromStr + fmt::Display + Eq + std::hash::Hash + Clone>
            (f: &Formula<PropositionalLanguage<Atom>>) -> bool {
        let mut iter = HashMapValuationIter::<PropositionalLanguage<Atom>>::from(get_atoms(f));
        loop {
            let result = evaluate::
                <
                PropositionalLanguage<Atom>,
                bool,
                BooleanInterpretation,
                HashMapValuation<PropositionalLanguage<Atom>, bool>
                >(f, &iter.values);
            if !result {
                return false;
            }
            if iter.next().is_none() {
                break;
            }
        }
        true
    }

    pub fn truth_table_antilogy
        <Atom: FromStr + fmt::Display + Eq + std::hash::Hash + Clone>
            (f: &Formula<PropositionalLanguage<Atom>>) -> bool {
        let mut iter = HashMapValuationIter::<PropositionalLanguage<Atom>>::from(get_atoms(f));
        loop {
            let result = evaluate::
                <
                PropositionalLanguage<Atom>,
                bool,
                BooleanInterpretation,
                HashMapValuation<PropositionalLanguage<Atom>, bool>
                >(f, &iter.values);
            if result {
                return false;
            }
            if iter.next().is_none() {
                break;
            }
        }
        true
    }

    pub fn truth_table_repr
        <Atom: FromStr + fmt::Display + Eq + std::hash::Hash + Clone, W: io::Write>
            (f: &Formula<PropositionalLanguage<Atom>>, writer: &mut W) -> io::Result<()> {
        let mut iter = HashMapValuationIter::<PropositionalLanguage<Atom>>::from(get_atoms(f));
        for a in iter.values.map.keys() {
            writer.write_all(format!("{}\t", a).as_bytes())?;
        }
        loop {
            let result = evaluate::
                <
                PropositionalLanguage<Atom>,
                bool,
                BooleanInterpretation,
                HashMapValuation<PropositionalLanguage<Atom>, bool>
                >(f, &iter.values);
            writer.write_all("\n".as_bytes())?;
            for a in iter.values.map.values() {
                writer.write_all(format!("{}\t", a).as_bytes())?;
            };
            writer.write_all(format!(" -> {}", result).as_bytes())?;
            if iter.next().is_none() {
                break;
            }
        }
        writer.write_all("\n".as_bytes())?;
        Ok(())
    }
    
    pub fn truth_table_print
        <Atom: FromStr + fmt::Display + Eq + std::hash::Hash + Clone>
            (f: &Formula<PropositionalLanguage<Atom>>) -> io::Result<()> {
        let stdout = std::io::stdout(); // get the global stdout entity
        let mut writer = std::io::BufWriter::new(stdout); // optional: wrap that handle in a buffer
        truth_table_repr(f, &mut writer)
    }
}
