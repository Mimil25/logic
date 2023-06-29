use std::{fmt, str::FromStr, marker::PhantomData};

use crate::formula::*;
use crate::opperators::*;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
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
    use std::ops::{Not, BitAnd, BitOr};

    use crate::formula::{Formula, Atom};
    use crate::opperators::And;

    use super::super::interpretation::*;
    use super::PropositionalLanguage;

    pub struct BooleanInterpretation;

    impl<
        Type: Copy + Not<Output = Type> + BitAnd<Output = Type> + BitOr<Output = Type> + Default,
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
                super::PLFuncs::Conjuction(_) => args.reduce(BitAnd::bitand).unwrap_or(!Type::default()),
                super::PLFuncs::Disjuction(_) => args.reduce(BitOr::bitor).unwrap_or_else(Type::default),
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
            // degradations of implications
            make_rule("({F} <=> {G})", "(({F} => {G}) && ({G} => {F}))"),
            make_rule("({F} => {G})", "(!{F} || {G})"),
            
            // simplification of double negation
            make_rule("!!{F}", "{F}"),
            
            // propagation of comutativity
            make_rule("({A} && {B})", "ALL({A}, {B})"),
            make_rule("({A} || {B})", "ANY({A}, {B})"),
            make_rule("ALL({*args*}, ALL({*args2*}))", "ALL({*args*}, {*args2*})"),
            make_rule("ANY({*args*}, ANY({*args2*}))", "ANY({*args*}, {*args2*})"),
            
            // simplification of double apparitions
            make_rule("ALL({*args*}, {A}, {A})", "ALL({*args*}, {A})"),
            make_rule("ANY({*args*}, {A}, {A})", "ANY({*args*}, {A})"),

            // simplification of atomic junctions
            make_rule("ALL({A})", "{A}"),
            make_rule("ANY({A})", "{A}"),
            
            // distribution of negation
            make_rule("!ALL({*args*})", "ANY({*args:ARG:!{ARG}*})"),
            make_rule("!ANY({*args*})", "AND({*args:ARG:!{ARG}*})"),

            // distribution of disjuctions
            make_rule(
                "ANY({*disjunction*}, {A}, ALL({*conjuction*}))",
                "ANY({*disjunction*}, ALL({*conjuction:ARG:ANY({A}, {ARG})*}))"),
            
            make_rule("ALL({*args*}, {A}, !{A})", "FALSE"),
            make_rule("ANY({*args*}, {A}, !{A})", "TRUE"),
            
            make_rule("ALL({*args*}, FALSE)", "FALSE"),
            make_rule("ANY({*args*}, TRUE)", "TRUE"),
            make_rule("ALL({*args*}, TRUE)", "ALL({*args*})"),
            make_rule("ANY({*args*}, FALSE)", "ANY({*args*})"),
            
            make_rule("!FALSE", "TRUE"),
            make_rule("!TRUE", "FALSE"),
        ];
        while rules.iter().any(|rule| replace(rule, f)) {}
    }

    

    //transform 
    pub fn resolve<A: Atom>(f: &mut Formula<PropositionalLanguage<A>>) {
        use super::PLFuncs;
        use super::super::replacement::*;
        
        to_cnf(f);

        println!("cnf : {}", f);
        

        if let Formula::Function(PLFuncs::Conjuction(_), clauses) = f {
            // saturation
            let devlopement_rules: Vec<ReplacementRule<PropositionalLanguage<A>>> = vec![
                make_rule(
                    "ALL(ANY({*b*}, !{A}), ANY({*c*}, {A}))",
                    "ANY({*b*}, {*c*})"),
                make_rule(
                    "ALL({A}, ANY({*b*}, !{A}))",
                    "ANY({*b*})"),
                make_rule(
                    "ALL(!{A}, ANY({*b*}, {A}))",
                    "ANY({*b*})"),
            ];

            loop {
                let mut a = 0;
                let mut b_max = a + 1;
                let mut new = false;
                while a < clauses.len() -1 {
                    let mut b = b_max;
                    b_max = clauses.len();
                    while b < clauses.len() {
                        let mut tmp = Formula::Function(
                            PLFuncs::Conjuction(crate::opperators::Conjuction),
                            vec![
                                clauses[a].clone(),
                                clauses[b].clone()
                            ]);
                        let n = devlopement_rules.iter().any(|rule| replace(rule, &mut tmp));
                        println!("{} {} + {} -> {}", n, clauses[a], clauses[b], tmp);
                        if n && !clauses.contains(&tmp) {
                            clauses.push(tmp);
                            new = true;
                        }
                        b += 1;
                    }
                    a += 1;
                }
                if !new {
                    break;
                }
            }
        }
        // simplification
        let simplification_rules: Vec<ReplacementRule<PropositionalLanguage<A>>> = vec![
            make_rule("ANY({*args*}, {A}, {A})", "ANY({*args*}, {A})"),
            make_rule("ALL({*args*}, {A}, {A})", "ALL({*args*}, {A})"),
            
            make_rule("ANY({A})", "{A}"),
            make_rule("ALL({A})", "{A}"),

            make_rule("ALL({*args*}, {A}, !{A})", "FALSE"),
            make_rule("ANY({*args*}, {A}, !{A})", "TRUE"),
            
            make_rule("ALL({*args*}, FALSE)", "FALSE"),
            make_rule("ANY({*args*}, TRUE)", "TRUE"),
            make_rule("ALL({*args*}, TRUE)", "ALL({*args*})"),
            make_rule("ANY({*args*}, FALSE)", "ANY({*args*})"),
            
            make_rule("!FALSE", "TRUE"),
            make_rule("!TRUE", "FALSE"),
            
            make_rule("ALL({*a*}, {A}, ANY({*b*}, {A}))", "ALL({*a*}, {A})"),
            make_rule("ALL({*a*}, {A}, ANY({*b*}, !{A}))", "ALL({*a*}, {A}, ANY({*b*}))"),
            make_rule("ALL({*a*}, !{A}, ANY({*b*}, {A}))", "ALL({*a*}, !{A}, ANY({*b*}))"),
        ];
        while simplification_rules.iter().any(|rule| replace(rule, f)){}
    }
}
