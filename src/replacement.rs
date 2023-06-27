use std::{collections::HashMap, borrow::Cow};

use crate::formula::*;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum PatternAtom<L: Language> {
    Atom(L::Atom), // must be an exact match -> "A"
    Any{
        name: String, // name of the replacement "{NAME}"
        id: usize,
    },
    AnyArgs { // -> {*NAME:ARG:pattern}
        name: String, // name of the repl
        arg: String,
        id: usize,
        pattern: Box<Formula<Pattern<L>>>,
    },
}

impl<L: Language> std::fmt::Display for PatternAtom<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PatternAtom::Any{name, id:_} => f.write_str(format!("{{{}}}", name).as_str()),
            PatternAtom::AnyArgs { name, id:_, arg, pattern } => f.write_str(format!("{{*{}:{}:{}}}", name, arg, pattern).as_str()),
            PatternAtom::Atom(atom) => (atom as &dyn std::fmt::Display).fmt(f),
        }
    }
}

impl<L: Language> std::str::FromStr for PatternAtom<L> {
    type Err = <L::Atom as std::str::FromStr>::Err;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() > 2 {
            if s.starts_with("{*") &&
                s.ends_with("*}") &&
                !s[2..].contains("{*") &&
                !s[..s.len()-2].contains("*}")
                {
                let mut split = s[2..s.len()-2].split(':');
                let name = split.next().unwrap();
                let arg = split.next().unwrap_or("ARG");
                let pattern = split.next().unwrap_or("{ARG}");
                return Ok(PatternAtom::AnyArgs {
                    name: name.to_string(),
                    arg: arg.to_string(),
                    pattern: Box::new(pattern.parse().unwrap()),
                    id: 0
                });
            } else if s.starts_with('{') &&
                s.ends_with('}') &&
                !s[1..].contains('{') &&
                !s[..s.len()-1].contains('}')
                {
                return Ok(PatternAtom::Any{name:s[1..s.len()-1].to_string(), id:0});
            }
        }
        L::Atom::from_str(s).map(|a| PatternAtom::Atom(a))
    }
}

impl<L: Language> Atom for PatternAtom<L> {}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
struct Pattern<L: Language> {
    _phantom_data: std::marker::PhantomData<L>,
}

impl<L: Language> Language for Pattern<L> {
    type Atom = PatternAtom<L>;
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
                PatternAtom::AnyArgs { name, arg:_, id, pattern:_ } |
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
                PatternAtom::AnyArgs { name, arg, id, pattern } => {
                    *id = ids.get(name.as_str())
                        .copied()
                        .unwrap();
                    pattern.get_atoms_mut()
                        .into_iter()
                        .for_each(|a| {
                            if let PatternAtom::Any { name, id } = a {
                                if name == arg {
                                    *id = rule.nb_pat_atoms;
                                } else {
                                    *id = ids.get(name.as_str())
                                        .copied()
                                        .unwrap();
                                }
                            }
                        });
                },
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
        (Formula::Atom(PatternAtom::AnyArgs { name:_, arg:_, id:_, pattern:_ }), _) => panic!("unexpected AnyArgs"),
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
        (Formula::Atom(PatternAtom::Atom(ap)), Formula::Atom(af)) => *ap == *af,
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
                return true;
            }
            if !p_args.is_empty() {
                if let Formula::Atom(PatternAtom::AnyArgs { name:_, arg:_, id, pattern }) = &p_args[0] {
                    match matches[*id] {
                        Some(_) => panic!("not implemented yet"),
                        None => {
                            // create a new match function if each arg in
                            // p_args[1..] can match one in f_args and all
                            // unmached args in f_args matche pattern else return false
                            // the created match function's args should contains
                            // args from f_args that have not been matched by one from p_args
                            
                            let mut used_args: Vec<bool> = vec![false; f_args.len()];
                            
                            let mut start = 0;

                            loop {
                                let mut tmp_matches = matches.clone();
                                let mut is_ok = true;
                                for p_arg in p_args[1..].iter() {
                                    let mut match_found = false;
                                    for i in start..f_args.len() {
                                        if !used_args[i] {
                                            let mut tmp_matches_2 = tmp_matches.clone();
                                            if match_rule(p_arg, &f_args[i], &mut tmp_matches_2) {
                                                used_args[i] = true;
                                                match_found = true;
                                                tmp_matches = tmp_matches_2;
                                                break;
                                            }
                                        }
                                    }
                                    if !match_found {
                                        is_ok = false;
                                        used_args.fill(false);
                                        break;
                                    }
                                }
                                if is_ok {
                                    *matches = tmp_matches;
                                    break;
                                }
                                start += 1;
                                if start + p_args.len() - 1 > f_args.len() {
                                    return false;
                                }
                            }
                            for (i, arg) in f_args.iter().enumerate() {
                                let mut phantom_matches = vec![None; matches.len() + 1];
                                if !used_args[i] && !match_rule(pattern, arg, &mut phantom_matches) {
                                    return false;
                                }
                            }
                            matches[*id] = Some(Cow::Owned(Formula::Function(
                                    ff.clone(),
                                    f_args.iter()
                                        .enumerate()
                                        .filter(|(i,_)| !used_args[*i])
                                        .map(|(_, a)| a)
                                        .cloned()
                                        .collect(),
                                    )));
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
            Some(Cow::Owned(a)) => a.clone(),
            Some(Cow::Borrowed(a)) => (*a).clone(),
            None => panic!("patern {} not matched", name)
        },
        Formula::Atom(PatternAtom::AnyArgs { name:_, arg:_, id:_, pattern:_ }) => panic!("unexpected AnyArgs"),
        Formula::Atom(PatternAtom::Atom(a)) => Formula::Atom(a.clone()),
        Formula::UnaryOpp(o, f) => Formula::UnaryOpp(
            o.clone(),
            Box::new(from_patern(f, matches))),
        Formula::BinaryOpp(a, o, b) => Formula::BinaryOpp(
            Box::new(from_patern(a, matches)),
            o.clone(),
            Box::new(from_patern(b, matches))),
        Formula::Function(f, args) => {
            if !args.is_empty() {
                if let Formula::Atom(PatternAtom::AnyArgs { name:_, arg:_, id, pattern }) = &args[0] {
                    if let Cow::Owned(Formula::Function(tmp_f, tmp_args)) = matches[*id].as_ref().unwrap() {
                        assert_eq!(f, tmp_f);
                        let mut tmp_matches = matches.clone();
                        let args = tmp_args.iter().map(|a| { // generic arguments from the matched
                                                      // function
                                tmp_matches.push(Some(Cow::Borrowed(a)));
                                let f = from_patern(pattern, &tmp_matches);
                                tmp_matches.pop();
                                f
                            })
                        .chain(
                            args[1..].iter().map(|a| { // direct argument of the pattern
                                from_patern(a, matches)
                            })).collect();
                        return Formula::Function(
                            f.clone(),
                            args,
                        );
                    } else {
                        panic!("{} should have been a function", matches[*id].as_ref().unwrap());
                    }
                }
                println!("here");
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
