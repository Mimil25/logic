use std::{fmt, str::FromStr, marker::PhantomData};

use crate::formula::*;
use crate::opperators::*;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PropositionalLanguage<A: Atom> {
    _phantom_data: PhantomData<A>
}

group_opps!(PLBinOpps, And, Or, Implie, IfAndOnlyIf);
group_opps!(PLFuncs, Conjuction, Disjuction);

impl<A: Atom> Language for PropositionalLanguage<A> {
    type Atom = A;
    type UnaryOpp = Neg;
    type BinaryOpp = PLBinOpps;
    type Function = PLFuncs;
}


pub mod boolean_interpretation {
    use std::io;
    use std::collections::HashSet;
    use std::ops::{Not, BitAnd, BitOr};

    use crate::formula::{Formula, Atom};
    use crate::opperators::Neg;

    use super::super::interpretation::*;
    use super::PropositionalLanguage;

    pub struct BooleanInterpretation;

    impl<
        Type: Copy + Not<Output = Type> + BitAnd<Output = Type> + BitOr<Output = Type>,
        A: Atom
    > Interpretation<PropositionalLanguage<A>, Type> for BooleanInterpretation {
        fn unary(_o: &<PropositionalLanguage<A> as crate::formula::Language>::UnaryOpp, a: Type) -> Type {
            !a
        }
        fn binary(a: Type, o: &<PropositionalLanguage<A> as crate::formula::Language>::BinaryOpp, b: Type) -> Type {
            match o {
                super::PLBinOpps::And(_) => a & b,
                super::PLBinOpps::Or(_) => a | b,
                super::PLBinOpps::Implie(_) => !a | b,
                super::PLBinOpps::IfAndOnlyIf(_) => (a & b) | (!a & !b),
            }
        }

        fn function<I: Iterator<Item = Type>>(f: &<PropositionalLanguage<A> as crate::formula::Language>::Function, args: I) -> Type {
            match f {
                super::PLFuncs::Conjuction(_) => args.reduce(BitAnd::bitand).unwrap(),
                super::PLFuncs::Disjuction(_) => args.reduce(BitOr::bitor).unwrap(),
            }
        }
    }


    pub fn truth_table_repr<A: Atom, W: io::Write>(f: &Formula<PropositionalLanguage<A>>, writer: &mut W) -> io::Result<()> {
        let mut iter = HashMapValuationIter::<A>::from(f.iter().cloned());
        let mut atoms = Vec::with_capacity(iter.values.data.len());
        for a in f.iter() {
            if !atoms.contains(&a) {
                writer.write_all(format!("{}\t", a).as_bytes())?;
                atoms.push(a);
            }
        }
        loop {
            let result = evaluate::
                <
                PropositionalLanguage<A>,
                bool,
                BooleanInterpretation,
                HashMapValuation<A, bool>
                >(f, &iter.values);
            writer.write_all("\n".as_bytes())?;
            for a in iter.values.data.iter() {
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
    
    pub fn truth_table_print<A: Atom>(f: &Formula<PropositionalLanguage<A>>) -> io::Result<()> {
        let stdout = std::io::stdout(); // get the global stdout entity
        let mut writer = std::io::BufWriter::new(stdout); // optional: wrap that handle in a buffer
        truth_table_repr(f, &mut writer)
    }

    pub fn to_cnf<A: Atom>(f: &mut Formula<PropositionalLanguage<A>>) {
        use super::super::replacement::*;
        let rules: Vec<ReplacementRule<PropositionalLanguage<A>>> = vec![ // TODO find a way to make
                                                                     // this static
            make_rule("({F} <=> {G})", "(({F} => {G}) && ({G} => {F}))"),
            make_rule("({F} => {G})", "(!{F} || {G})"),
            make_rule("!!{F}", "{F}"),
            make_rule("!({F} && {G})", "(!{F} || !{G})"),
            make_rule("!({F} || {G})", "(!{F} && !{G})"),
            make_rule("({F} || ({G} && {H}))", "(({F} || {G}) && ({F} || {H}))"),
            make_rule("(({F} && {G}) || {H})", "(({F} || {H}) && ({G} || {H}))"),
            make_rule("({F} || {F})", "{F}"),
        ];
        loop {
            let changes = rules.iter()
                .map(|rule| replace(rule, f))
                .reduce(|a, b| a|b)
                .unwrap_or(false);
            if !changes {
                break;
            }
        }
    }
    
    pub fn to_dnf<A: Atom>(f: &mut Formula<PropositionalLanguage<A>>) {
        use super::super::replacement::*;
        let rules: Vec<ReplacementRule<PropositionalLanguage<A>>> = vec![ // TODO find a way to make
                                                                     // this static
            make_rule("!!{F}", "{F}"),
            make_rule("!({F} && {G})", "(!{F} || !{G})"),
            make_rule("!({F} || {G})", "(!{F} && !{G})"),
            make_rule("({F} || {F})", "{F}"),
        ];
        let mut n = f.to_owned();
        n = Formula::UnaryOpp(Neg, Box::new(n));
        to_cnf(f);
        n = Formula::UnaryOpp(Neg, Box::new(n));
        *f = n;
        loop {
            let changes = rules.iter()
                .map(|rule| replace(rule, f))
                .reduce(|a, b| a|b)
                .unwrap_or(false);
            if !changes {
                break;
            }
        }
    }
}
