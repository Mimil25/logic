use std::collections::HashMap;

use crate::formula::*;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum PaternAtom<A: Atom> {
    Atom(A), // must be an exact match
    Any{
        name: String, // name of the replacement
        id: usize,
    },
}

impl<A: Atom> std::fmt::Display for PaternAtom<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PaternAtom::Any{name, id} => f.write_str(format!("{{{}:{}}}", name, id).as_str()),
            PaternAtom::Atom(atom) => atom.fmt(f),
        }
    }
}

impl<A: Atom> std::str::FromStr for PaternAtom<A> {
    type Err = A::Err;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() > 2 &&
            s.starts_with('{') &&
            s.ends_with('}') &&
            !s[1..].contains('{') &&
            !s[..(s.len() - 1)].contains('}') {
            
            Ok(PaternAtom::Any{name:s[1..s.len()-1].to_string(), id:0})
        } else {
            A::from_str(s).map(|a| PaternAtom::Atom(a))
        }
    }
}

impl<A: Atom> Atom for PaternAtom<A> {}

#[derive(PartialEq, Eq, Clone)]
struct Patern<L: Language> {
    _phantom_data: std::marker::PhantomData<L>,
}

impl<L: Language> Language for Patern<L> {
    type Atom = PaternAtom<L::Atom>;
    type UnaryOpp = L::UnaryOpp;
    type BinaryOpp = L::BinaryOpp;
    type Function = L::Function;
}

pub struct ReplacementRule<L: Language> {
    pat: Formula<Patern<L>>,
    replacement: Formula<Patern<L>>,
    nb_pat_atoms: usize
}

impl<L: Language> std::fmt::Display for ReplacementRule<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{} ~> {}", self.pat, self.replacement))
    }
}

pub fn make_rule<L: Language>(pat: &str, replacement: &str) -> ReplacementRule<L> {
    let mut rule = ReplacementRule {
        pat: pat.parse().unwrap(),
        replacement: replacement.parse().unwrap(),
        nb_pat_atoms: 0,
    };
    let mut ids = HashMap::new();
    get_atoms_mut(&mut rule.pat)
        .into_iter()
        .for_each(|a| {
            match a {
                PaternAtom::Atom(_) => {},
                PaternAtom::Any { name, id } => {
                    *id = ids.get(name.as_str())
                        .copied()
                        .unwrap_or_else(|| {
                            let id = ids.len();
                            ids.insert(name.as_str(), id);
                            id
                        });
                }
            }
        });
    rule.nb_pat_atoms = ids.len();
    get_atoms_mut(&mut rule.replacement)
        .into_iter()
        .for_each(|a| {
            match a {
                PaternAtom::Atom(_) => {},
                PaternAtom::Any { name, id } => {
                    *id = ids.get(name.as_str())
                        .copied()
                        .unwrap();
                }
            }
        });
    rule
}

fn match_rule<'a, L: Language>(pat: &Formula<Patern<L>>, f: &'a Formula<L>, matches: &mut Vec<Option<&'a Formula<L>>>) -> bool {
    match (pat, f) {
        (Formula::Atom(PaternAtom::Any { name:_, id }), f) => {
            match matches[*id] {
                Some(f2) => {
                    return *f == *f2;
                },
                None => {
                    matches[*id] = Some(f);
                }
            }
            true
        },
        (Formula::UnaryOpp(op, p), Formula::UnaryOpp(of, f)) => {
            if op != of {
                return false;
            }
            match_rule(p, f, matches)
        },
        (Formula::BinaryOpp(p1, op, p2), Formula::BinaryOpp(f1, of, f2)) => {
            if op != of {
                return false;
            }
            match_rule(p1, f1, matches) && match_rule(p2, f2, matches)
        }
        (Formula::Function(fp, p_args), Formula::Function(ff, f_args)) => {
            if fp != ff {
                return false;
            }
            return p_args
                .iter()
                .zip(f_args.iter())
                .all(|(p, f)| match_rule(p, f, matches));
        },
        (_, _) => false,
    }
}

fn from_patern<L: Language>(pat: &Formula<Patern<L>>, matches: &Vec<Option<&Formula<L>>>) -> Formula<L> {
    match pat {
        Formula::Atom(PaternAtom::Any { name:_, id }) => matches[*id].unwrap().clone(),
        Formula::Atom(PaternAtom::Atom(a)) => Formula::Atom(a.clone()),
        Formula::UnaryOpp(o, f) => Formula::UnaryOpp(
            o.clone(),
            Box::new(from_patern(f, matches))),
        Formula::BinaryOpp(a, o, b) => Formula::BinaryOpp(
            Box::new(from_patern(a, matches)),
            o.clone(),
            Box::new(from_patern(b, matches))),
        Formula::Function(f, args) => Formula::Function(
            f.clone(),
            args.iter().map(|a| from_patern(a, matches)).collect()),
    }
}

pub fn replace<L: Language>(rule: &ReplacementRule<L>, f: &mut Formula<L>) -> bool {
    let mut matches = Vec::with_capacity(rule.nb_pat_atoms);
    matches.resize_with(rule.nb_pat_atoms, || None);
    let mut changes = match_rule(&rule.pat, f, &mut matches);
    if changes {
        *f = from_patern(&rule.replacement, &matches);
    }
    changes |= match f {
        Formula::Atom(_) => false,
        Formula::UnaryOpp(_, a) => replace(rule, a),
        Formula::BinaryOpp(a, _, b) => replace(rule, a) | replace(rule, b),
        Formula::Function(_, args) => args
            .iter_mut()
            .map(|a| replace(rule, a))
            .reduce(|a, b| a | b)
            .unwrap_or(false),
    };
    changes
}
