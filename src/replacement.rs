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

// return true if pattern a can match any formula that can be matched by pattern b
fn pattern_include<L: Language>(a: &Formula<Pattern<L>>, b: &Formula<Pattern<L>>) -> bool {
    let (p_a, nb_pat_atoms) = pattern_to_pattern_pattern(a);
    let mut _phantom_matches = vec![None; nb_pat_atoms];
    if let MatchResult::Match = match_rule(&p_a, b, &mut _phantom_matches) {
        return true;
    } else {
        return false;
    }
}

fn rearange_func_pattern<L: Language>(pattern: &mut Formula<Pattern<L>>) {
    match pattern {
        Formula::UnaryOpp(_, p) => rearange_func_pattern(&mut*p),
        Formula::BinaryOpp(a, _, b) => {
            rearange_func_pattern(&mut*a);
            rearange_func_pattern(&mut*b);
        },
        Formula::Function(_, args) => {
            let mut start = 0;
            if let Formula::Atom(PatternAtom::AnyArgs { name:_, arg:_, id:_, pattern }) = &mut args[0] {
                rearange_func_pattern(&mut*pattern);
                if args.len() < 3 {
                    return;
                }
                start = 1;
            } else if args.len() < 2 {
                return;
            }
            let start = start;
            for p in args[start..].iter_mut() {
                rearange_func_pattern(p);
            }
            args[start..].sort_unstable_by(|a, b| {
                if pattern_include(a, b) {
                    Ordering::Greater
                } else if pattern_include(b, a) {
                    Ordering::Less
                } else {
                    Ordering::Equal
                }
            });
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

fn match_args<'a, L: Language> (
    p_args: &[Formula<Pattern<L>>],
    f_args: &'a[Formula<L>],
    matches: &mut Vec<Option<Cow<'a, Formula<L>>>>
    ) -> (MatchResult, Vec<bool>) {
    debug_assert!(f_args.is_sorted_by(|a, b| Some(cmp_formula(a, b))));
    let mut used_args: Vec<bool> = vec![false; f_args.len()];

    let mut start = vec![0; p_args.len()];
    let matches_save = matches.clone();

    loop {
        let mut all_matched = true;
        for (i, p_arg) in p_args.iter().enumerate() {

            let mut j = start[i];
            while j < f_args.len() && used_args[j] {
                j += 1;
            }
            if j == f_args.len() {
                all_matched = false;
                break;
            }
            match match_rule(p_arg, &f_args[j], matches) {
                MatchResult::Match => {
                    used_args[j] = true;
                },
                MatchResult::CantMatch |
                    MatchResult::MayMatchIfDifferent(_) => {
                        all_matched = false;
                        break;
                    },
            }
        }

        if all_matched {
            return (MatchResult::Match, used_args);
        } else {
            for i in 0..matches.len() {
                if matches_save[i].is_none() {
                    matches[i] = None;
                }
            }
            used_args.fill(false);
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
            return (MatchResult::CantMatch, used_args);
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
                if let Formula::Atom(PatternAtom::AnyArgs { name:_, arg:_, id, pattern:_ }) = &p_args[0] {
                    match matches[*id] {
                        Some(_) => panic!("not implemented yet"),
                        None => {
                            // create a new match function if each arg in
                            // p_args[1..] can match one in f_args and all
                            // unmached args in f_args matche pattern else return false
                            // the created match function's args should contains
                            // args from f_args that have not been matched by one from p_args

                            let (mr, used_args) = match_args(&p_args[1..], f_args, matches);
                            match mr {
                                MatchResult::Match => {},
                                MatchResult::CantMatch => return MatchResult::CantMatch,
                                MatchResult::MayMatchIfDifferent(id) => return MatchResult::MayMatchIfDifferent(id),
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
            return match_args(p_args, f_args, matches).0;
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
        Formula::UnaryOpp(o, f) => Formula::UnaryOpp(o.clone(), Box::new(from_patern(f, matches))),
        Formula::BinaryOpp(a, o, b) => Formula::BinaryOpp(Box::new(from_patern(a, matches)), o.clone(), Box::new(from_patern(b, matches))),
        Formula::Function(f, args) => {
            Formula::Function(
                f.clone(),
                args.iter().map(|a| {
                    if let Formula::Atom(PatternAtom::AnyArgs { name:_, arg:_, id, pattern }) = a {
                        if let Cow::Owned(Formula::Function(_, tmp_args)) = matches[*id].as_ref().unwrap() {
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
                        Ok(iter) => iter.for_each(|f| {
                            let p = v.binary_search_by(|probe|
                                cmp_formula(probe, &f)
                                ).unwrap_or_else(|e| e);
                            v.insert(p, f);
                        }),
                        Err(iter) => iter.for_each(|f| {
                            let p = v.binary_search_by(|probe|
                                cmp_formula(probe, &f)
                                ).unwrap_or_else(|e| e);
                            v.insert(p, f);
                        }),
                    }
                    debug_assert!(v.is_sorted_by(|a, b| Some(cmp_formula(a, b))));
                    v
                })
            )
        },
    }
}

fn cmp_formula<L1: Language, L2: Language>(a: &Formula<L1>, b: &Formula<L2>) -> Ordering
where L1::UnaryOpp: PartialOrd<L2::UnaryOpp>,
      L1::BinaryOpp: PartialOrd<L2::BinaryOpp>,
      L1::Function: PartialOrd<L2::Function> {
          match (a, b) {
              (Formula::Atom(_), Formula::Atom(_)) => Ordering::Equal,
              (Formula::Atom(_), Formula::UnaryOpp(_, _)) => Ordering::Greater,
              (Formula::Atom(_), Formula::BinaryOpp(_, _, _)) => Ordering::Greater,
              (Formula::Atom(_), Formula::Function(_, _)) => Ordering::Greater,

              (Formula::UnaryOpp(_, _), Formula::Atom(_)) => Ordering::Less,
              (Formula::UnaryOpp(a, _), Formula::UnaryOpp(b, _)) => a.partial_cmp(b).unwrap_or(Ordering::Equal),
              (Formula::UnaryOpp(_, _), Formula::BinaryOpp(_, _, _)) => Ordering::Greater,
              (Formula::UnaryOpp(_, _), Formula::Function(_, _)) => Ordering::Greater,

              (Formula::BinaryOpp(_, _, _), Formula::Atom(_)) => Ordering::Less,
              (Formula::BinaryOpp(_, _, _), Formula::UnaryOpp(_, _)) => Ordering::Less,
              (Formula::BinaryOpp(_, a, _), Formula::BinaryOpp(_, b, _)) => a.partial_cmp(b).unwrap_or(Ordering::Equal),
              (Formula::BinaryOpp(_, _, _), Formula::Function(_, _)) => Ordering::Greater,

              (Formula::Function(_, _), Formula::Atom(_)) => Ordering::Less,
              (Formula::Function(_, _), Formula::UnaryOpp(_, _)) => Ordering::Less,
              (Formula::Function(_, _), Formula::BinaryOpp(_, _, _)) => Ordering::Less,
              (Formula::Function(a, _), Formula::Function(b, _)) => a.partial_cmp(b).unwrap_or(Ordering::Equal),
          }
      }

fn sort_formula_func<L: Language>(f: &mut Formula<L>) {
    match f {
        Formula::UnaryOpp(_, a) => sort_formula_func(a),
        Formula::BinaryOpp(a, _, b) => {
            sort_formula_func(a);
            sort_formula_func(b);
        },
        Formula::Function(_, args) => {
            args.iter_mut().for_each(|arg| sort_formula_func(arg));
            args.sort_unstable_by(cmp_formula);
            debug_assert!(args.is_sorted_by(|a, b| Some(cmp_formula(a, b))));
        },
        _ => {},
    }
}

pub fn apply<L: Language>(rules: &[ReplacementRule<L>], f: &Formula<L>) -> Formula<L> {
    let mut f = f.clone();
    sort_formula_func(&mut f);

    while rules.iter().any(|rule| {
        let b = replace_top_down(rule, &mut f);
        println!("{} {}", rule, b);
        b
    }) {
    }

    f
}

fn replace_top_down<L: Language>(rule: &ReplacementRule<L>, f: &mut Formula<L>) -> bool {
    let mut matches = Vec::with_capacity(rule.nb_pat_atoms);
    matches.resize_with(rule.nb_pat_atoms, || None);
    let changes = match f {
        Formula::Atom(_) => false,
        Formula::UnaryOpp(_, a) => replace_top_down(rule, a),
        Formula::BinaryOpp(a, _, b) => replace_top_down(rule, a) | replace_top_down(rule, b),
        Formula::Function(_, args) => {
            let b = args
                .iter_mut()
                .map(|a| replace_top_down(rule, a))
                .reduce(|a, b| a | b)
                .unwrap_or(false);
            if b {
                args.sort_unstable_by(cmp_formula);
            }
            b
        },
    };
    let s_changes = match_rule(&rule.pat, f, &mut matches).is_match();
    if s_changes {
        *f = from_patern(&rule.replacement, &matches);
    }
    changes | s_changes
}
