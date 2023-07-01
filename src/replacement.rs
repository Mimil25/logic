use std::{collections::HashMap, borrow::Cow, cmp::Ordering};

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
            PatternAtom::AnyArgs { name, id:_, arg, pattern } => f.write_str(format!("{{*{}:{}:{}*}}", name, arg, pattern).as_str()),
            PatternAtom::Atom(atom) => (atom as &dyn std::fmt::Display).fmt(f),
        }
    }
}

impl<L: Language> TryFrom<(&[&str], &mut usize)> for PatternAtom<L> {
    type Error = String;
    fn try_from(value: (&[&str], &mut usize)) -> Result<Self, Self::Error> {
        match value.0 {
            [] => Err(String::from("unexpected end while parsing PatternAtom")),
            ["{", name, "}", ..] => {
                if name.chars().all(|c| c.is_digit(36)) {
                    *value.1 = 3;
                    Ok(PatternAtom::Any { name: name.to_string(), id: 0 })
                } else {
                    Err(format!("identifier {} should be alphanumeric 0", name))
                }
            },
            ["{","*",name, "*","}", ..] => {
                if name.chars().all(|c| c.is_digit(36)) {
                    *value.1 = 5;
                    Ok(PatternAtom::AnyArgs {
                        name: name.to_string(),
                        arg: String::from("arg"),
                        id: 0,
                        pattern: Box::new(Formula::Atom(PatternAtom::Any { name: String::from("arg"), id: 0 })),
                    })
                } else {
                    Err(format!("identifier {} should be alphanumeric 1", name))
                }
            },
            ["{","*",name,":", arg, ":", pattern_s@..] => {
                if name.chars().all(|c| c.is_digit(36)) {
                    let mut len_pattern = 0;
                    let pattern = Formula::try_from((pattern_s, &mut len_pattern))?;
                    if len_pattern + 1 >= pattern_s.len() || 
                        pattern_s[len_pattern] != "*" ||
                            pattern_s[len_pattern + 1] != "}" {
                        return Err(format!(
                                "unexpected '{}{}', '*}}' expected to close AnyArgs pattern",
                                pattern_s[len_pattern], pattern_s[len_pattern + 1]));
                    }
                    *value.1 = 8 + len_pattern;
                    Ok(PatternAtom::AnyArgs {
                        name: name.to_string(),
                        arg: arg.to_string(),
                        id: 0,
                        pattern: Box::new(pattern),
                    })
                } else {
                    Err(format!("identifier {} should be alphanumeric 2", name))
                }
            },
            [..] => L::Atom::try_from(value).map(|atom| PatternAtom::Atom(atom)),
        }
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

fn pattern_to_pattern_pattern<L: Language>(pattern: &Formula<Pattern<L>>) -> (Formula<Pattern<Pattern<L>>>, usize) {
    // TODO find a nicer way to do this
    let repr = format!("{}", pattern);
    let mut p = repr.parse().unwrap();
    let nb_pat_atoms = give_ids_to_pattern(&mut p).len();
    (p, nb_pat_atoms)
}

fn rearange_func_pattern<L: Language>(pattern: &mut Formula<Pattern<L>>) {
    match pattern {
        Formula::UnaryOpp(_, p) => rearange_func_pattern(&mut*p),
        Formula::BinaryOpp(a, _, b) => {
            rearange_func_pattern(&mut*a);
            rearange_func_pattern(&mut*b);
        },
        Formula::Function(_, args) => {
            if args.len() < 3 {
                return;
            }
            if let Formula::Atom(PatternAtom::AnyArgs { name:_, arg:_, id:_, pattern }) = &mut args[0] {
                rearange_func_pattern(&mut*pattern);
                for p in args[1..].iter_mut() {
                    rearange_func_pattern(p);
                }
                args[1..].sort_unstable_by(|a, b| {
                    let (p_a, nb_pat_atoms) = pattern_to_pattern_pattern(a);
                    let mut _phantom_matches = vec![None; nb_pat_atoms];
                    if let MatchResult::Match = match_rule(&p_a, b, &mut _phantom_matches) {
                        return Ordering::Greater;
                    }
                    let (p_b, nb_pat_atoms) = pattern_to_pattern_pattern(b);
                    _phantom_matches.fill(None);
                    _phantom_matches.resize(nb_pat_atoms, None);
                    if let MatchResult::Match = match_rule(&p_b, a, &mut _phantom_matches) {
                        return Ordering::Less;
                    }
                    Ordering::Equal
                });
            }
        },
        _ => {},
    }
}

fn give_ids_to_pattern<L: Language>(pattern: &mut Formula<Pattern<L>>) -> HashMap<&str, usize> {
    let mut ids = HashMap::new();
    pattern.get_atoms_mut()
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
    ids
}

pub fn make_rule<L: Language>(pat: &str, replacement: &str) -> ReplacementRule<L> {
    let mut rule = ReplacementRule {
        pat: pat.parse().unwrap(),
        replacement: replacement.parse().unwrap(),
        nb_pat_atoms: 0,
    };
    rearange_func_pattern(&mut rule.pat);
    let ids = give_ids_to_pattern(&mut rule.pat);
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

enum MatchResult {
    Match,
    MayMatchIfDifferent(usize),
    CantMatch,
}

impl MatchResult {
    fn is_match(&self) -> bool {
        match self {
            MatchResult::Match => true,
            _ => false,
        }
    }
}

fn match_rule<'a, L: Language>(pat: &Formula<Pattern<L>>, f: &'a Formula<L>, matches: &mut Vec<Option<Cow<'a, Formula<L>>>>) -> MatchResult {
    match (pat, f) {
        (Formula::Atom(PatternAtom::AnyArgs { name:_, arg:_, id:_, pattern:_ }), _) => panic!("unexpected AnyArgs"),
        (Formula::Atom(PatternAtom::Any { name:_, id }), f) => {
            match &matches[*id] {
                Some(f2) => {
                    if *f == **f2 {
                        MatchResult::Match
                    } else {
                        MatchResult::MayMatchIfDifferent(*id)
                    }
                },
                None => {
                    matches[*id] = Some(Cow::Borrowed(f));
                    MatchResult::Match
                }
            }
        },
        (Formula::Atom(PatternAtom::Atom(ap)), Formula::Atom(af)) => {
            if *ap == *af {
                MatchResult::Match
            } else {
                MatchResult::CantMatch
            }
        },
        (Formula::UnaryOpp(op, p), Formula::UnaryOpp(of, f)) => {
            if op != of {
                return MatchResult::CantMatch;
            }
            match_rule(p, f, matches)
        },
        (Formula::BinaryOpp(p1, op, p2), Formula::BinaryOpp(f1, of, f2)) => {
            if op != of {
                return MatchResult::CantMatch;
            }
            let mr = match_rule(p1, f1, matches);
            match mr {
                MatchResult::Match => match_rule(p2, f2, matches),
                MatchResult::CantMatch => MatchResult::CantMatch,
                MatchResult::MayMatchIfDifferent(id) => MatchResult::MayMatchIfDifferent(id),
            }
        }
        (Formula::Function(fp, p_args), Formula::Function(ff, f_args)) => {
            if fp != ff {
                return MatchResult::CantMatch;
            }
            if p_args.is_empty() && f_args.is_empty() {
                return MatchResult::Match;
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
                            
                            let mut start = vec![0; p_args.len()];
                            let mut tmp_matches = matches.clone();
                            let mut tmp_matches_2 = tmp_matches.clone();

                            loop {

                                let mut is_ok = true;
                                for (i_arg, p_arg) in p_args[1..].iter().enumerate() {
                                    let mut match_found = false;
                                    let mut only_cant_match = true;
                                    let mut ret_if_cant_match = MatchResult::CantMatch;
                                    for i in start[i_arg]..f_args.len() {
                                        if !used_args[i] {
                                            let mr = match_rule(p_arg, &f_args[i], &mut tmp_matches_2);
                                            match mr {
                                                MatchResult::Match => {
                                                    used_args[i] = true;
                                                    match_found = true;
                                                    only_cant_match = false;
                                                    for i in 0..tmp_matches.len() {
                                                        if tmp_matches[i].is_none() && tmp_matches_2[i].is_some() {
                                                            tmp_matches[i] = Some(tmp_matches_2[i].as_ref().unwrap().clone());
                                                        }
                                                    }
                                                    break;
                                                },
                                                MatchResult::CantMatch => {},
                                                MatchResult::MayMatchIfDifferent(id) => {
                                                    if matches[id].is_none() { // that mean its one
                                                                               // of p_args
                                                        only_cant_match = false;
                                                    } else {
                                                        ret_if_cant_match = MatchResult::MayMatchIfDifferent(id);
                                                    }
                                                }
                                            }
                                            for i in 0..tmp_matches.len() {
                                                if tmp_matches[i].is_none() {
                                                    tmp_matches_2[i] = None;
                                                }
                                            }
                                        }
                                    }
                                    if only_cant_match {
                                        return ret_if_cant_match;
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
                                
                                //increment start
                                let mut end = true;
                                for d in start.iter_mut() {
                                    *d += 1;
                                    if *d >= f_args.len() {
                                        *d = 0;
                                    } else {
                                        end = false;
                                        break;
                                    }
                                }
                                if end {
                                    return MatchResult::CantMatch;
                                }
                                for i in 0..matches.len() {
                                    if matches[i].is_none() {
                                        tmp_matches[i] = None;
                                        tmp_matches_2[i] = None;
                                    }
                                }
                            }
                            for (i, arg) in f_args.iter().enumerate() {
                                let mut phantom_matches = vec![None; matches.len() + 1];
                                if !used_args[i] {
                                    let mr = match_rule(pattern, arg, &mut phantom_matches);
                                    match mr {
                                        MatchResult::Match => {},
                                        MatchResult::CantMatch => return MatchResult::CantMatch,
                                        MatchResult::MayMatchIfDifferent(id) => return MatchResult::MayMatchIfDifferent(id),
                                    }
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
                            return MatchResult::Match;
                        }
                    }
                }
            }
            if p_args.len() != f_args.len() {
                return MatchResult::CantMatch;
            }
            for (p, f) in p_args.iter().zip(f_args.iter()) {
                match match_rule(p, f, matches) {
                    MatchResult::Match => {},
                    MatchResult::CantMatch => return MatchResult::CantMatch,
                    MatchResult::MayMatchIfDifferent(id) => return MatchResult::MayMatchIfDifferent(id),
                }
            }
            return MatchResult::Match;
        },
        (_, _) => MatchResult::CantMatch,
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
            Formula::Function(
                f.clone(),
                args.iter().map(|a| {
                    if let Formula::Atom(PatternAtom::AnyArgs { name:_, arg:_, id, pattern }) = a {
                        if let Cow::Owned(Formula::Function(tmp_f, tmp_args)) = matches[*id].as_ref().unwrap() {
                            let mut tmp_matches = matches.clone();
                            let args = tmp_args.iter().map(move |a| { // generic arguments from the matched
                                                        // function
                                    tmp_matches.push(Some(Cow::Borrowed(a)));
                                    let f = from_patern(pattern, &tmp_matches);
                                    tmp_matches.pop();
                                    f
                                }).collect::<Vec<_>>().into_iter();
                            Err(args)
                        } else {
                            panic!("{} should have been a function", matches[*id].as_ref().unwrap());
                        }
                    } else {
                        Ok([from_patern(a, matches)].into_iter())
                    }
                }).fold(Vec::new(), |mut v, iter| {
                    match iter {
                        Ok(iter) => iter.for_each(|f| v.push(f)),
                        Err(iter) => iter.for_each(|f| v.push(f))
                    }
                    v
                })
            )
        },
    }
}

pub fn replace<L: Language>(rule: &ReplacementRule<L>, f: &mut Formula<L>) -> bool {
    let mut matches = Vec::with_capacity(rule.nb_pat_atoms);
    matches.resize_with(rule.nb_pat_atoms, || None);
    let s_changes = match_rule(&rule.pat, f, &mut matches).is_match();
    if s_changes {
        *f = from_patern(&rule.replacement, &matches);
    }
    let changes = match f {
        Formula::Atom(_) => false,
        Formula::UnaryOpp(_, a) => replace(rule, a),
        Formula::BinaryOpp(a, _, b) => replace(rule, a) | replace(rule, b),
        Formula::Function(_, args) => args
            .iter_mut()
            .map(|a| replace(rule, a))
            .reduce(|a, b| a | b)
            .unwrap_or(false),
    };
    changes | s_changes
}
