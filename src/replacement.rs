use std::{collections::HashMap, borrow::Cow};

use crate::formula::*;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum PatternAtom<A: Atom> {
    Atom(A), // must be an exact match
    Any{
        name: String, // name of the replacement
        id: usize,
    },
    AnyArgs {
        name: String, // name of the replacement
        id: usize,
    },
}

impl<A: Atom> std::fmt::Display for PatternAtom<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PatternAtom::Any{name, id} => f.write_str(format!("{{{}:{}}}", name, id).as_str()),
            PatternAtom::AnyArgs { name, id } => f.write_str(format!("{{*{}:{}}}", name, id).as_str()),
            PatternAtom::Atom(atom) => (atom as &dyn std::fmt::Display).fmt(f),
        }
    }
}

impl<A: Atom> std::str::FromStr for PatternAtom<A> {
    type Err = A::Err;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() > 2 &&
            s.starts_with('{') &&
            s.ends_with('}') &&
            !s[1..].contains('{') &&
            !s[..(s.len() - 1)].contains('}') {
            if s.starts_with("{*") {
                Ok(PatternAtom::AnyArgs {name:s[2..s.len()-1].to_string(), id:0})
            } else {
                Ok(PatternAtom::Any{name:s[1..s.len()-1].to_string(), id:0})
            }
        } else {
            A::from_str(s).map(|a| PatternAtom::Atom(a))
        }
    }
}

impl<A: Atom> Atom for PatternAtom<A> {}

#[derive(Debug, PartialEq, Eq, Clone)]
struct Pattern<L: Language> {
    _phantom_data: std::marker::PhantomData<L>,
}

impl<L: Language> Language for Pattern<L> {
    type Atom = PatternAtom<L::Atom>;
    type UnaryOpp = L::UnaryOpp;
    type BinaryOpp = L::BinaryOpp;
    type Function = L::Function;
}

pub struct ReplacementRule<L: Language> {
    pat: Formula<Pattern<L>>,
    replacement: Formula<Pattern<L>>,
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
    rule.pat.get_atoms_mut()
        .into_iter()
        .for_each(|a| {
            match a {
                PatternAtom::Atom(_) => {},
                PatternAtom::AnyArgs { name, id } |
                PatternAtom::Any { name, id } => {
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
    rule.replacement.get_atoms_mut()
        .into_iter()
        .for_each(|a| {
            match a {
                PatternAtom::Atom(_) => {},
                PatternAtom::AnyArgs { name, id } |
                PatternAtom::Any { name, id } => {
                    *id = ids.get(name.as_str())
                        .copied()
                        .unwrap();
                }
            }
        });
    rule
}

fn match_rule<'a, L: Language>(pat: &Formula<Pattern<L>>, f: &'a Formula<L>, matches: &mut Vec<Option<Cow<'a, Formula<L>>>>) -> bool {
    match (pat, f) {
        (Formula::Atom(PatternAtom::Any { name:_, id }), f) => {
            match &matches[*id] {
                Some(f2) => {
                    return *f == **f2;
                },
                None => {
                    matches[*id] = Some(Cow::Borrowed(f));
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
            if p_args.is_empty() && f_args.is_empty() {
            }
            if !p_args.is_empty() {
                if let Formula::Atom(PatternAtom::AnyArgs { name:_, id }) = p_args[0] {
                    println!("args pattern matched");
                    match matches[id] {
                        Some(_) => panic!("not implemented yet"),
                        None => {
                            let mut used_args: Vec<usize> = Vec::new();
                            for p_arg in p_args[1..].iter() {
                                let mut match_found = false;
                                for (i, f_arg) in f_args.iter().enumerate() {
                                    if !used_args.contains(&i) {
                                        let mut tmp_matches = matches.clone();
                                        if match_rule(p_arg, f_arg, &mut tmp_matches) {
                                            used_args.push(i);
                                            match_found = true;
                                            *matches = tmp_matches;
                                            break;
                                        }
                                    }
                                }
                                if !match_found {
                                    println!("args pattern didn't match");
                                    return false;
                                }
                            }
                            matches[id] = Some(Cow::Owned(Formula::Function(
                                    ff.clone(),
                                    f_args.iter()
                                        .enumerate()
                                        .filter(|(i,_)| !used_args.contains(i))
                                        .map(|(_, a)| a)
                                        .cloned()
                                        .collect(),
                                    )));
                            println!("match : {}", *matches[id].as_ref().unwrap());
                            return true;
                        }
                    }
                }
            }
            return p_args
                .iter()
                .zip(f_args.iter())
                .all(|(p, f)| match_rule(p, f, matches));
        },
        (_, _) => false,
    }
}

fn from_patern<L: Language>(pat: &Formula<Pattern<L>>, matches: &Vec<Option<Cow<Formula<L>>>>) -> Formula<L> {
    match pat {
        Formula::Atom(PatternAtom::Any { name, id }) => match &matches[*id] {
            Some(Cow::Owned(a)) => a.clone().to_owned(),
            Some(Cow::Borrowed(a)) => a.clone().to_owned(),
            None => panic!("patern {} not matched", name)
        },
        Formula::Atom(PatternAtom::AnyArgs { name:_, id:_ }) => panic!("unexpected AnyArgs"),
        Formula::Atom(PatternAtom::Atom(a)) => Formula::Atom(a.clone()),
        Formula::UnaryOpp(o, f) => Formula::UnaryOpp(
            o.clone(),
            Box::new(from_patern(f, matches))),
        Formula::BinaryOpp(a, o, b) => Formula::BinaryOpp(
            Box::new(from_patern(a, matches)),
            o.clone(),
            Box::new(from_patern(b, matches))),
        Formula::Function(f, args) => {
            println!("here");
            if !args.is_empty() {
                if let Formula::Atom(PatternAtom::AnyArgs { name:_, id }) = args[0] {
                    if let Cow::Owned(Formula::Function(tmp_f, tmp_args)) = matches[id].as_ref().unwrap() {
                        assert_eq!(f, tmp_f);
                        let args = [tmp_args.clone(), args[1..].iter().map(|a| from_patern(a, matches)).collect()].concat();
                        return Formula::Function(
                            f.clone(),
                            args,
                        );
                    } else {
                        panic!("{} should have been a function", matches[id].as_ref().unwrap());
                    }
                }
            }
            Formula::Function(
                f.clone(),
                args.iter().map(|a| from_patern(a, matches)).collect()
            )
        },
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
