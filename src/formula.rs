use std::{fmt, str::FromStr, collections::HashSet};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Variable {
    name: String,
}

pub trait Atom: fmt::Display + Clone + PartialEq + Eq + FromStr + std::hash::Hash{}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name.as_str())
    }
}

impl FromStr for Variable {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.chars().all(char::is_uppercase) {
            Ok(Variable { name: s.to_string() })
        } else {
            Err(())
        }
    }
}

impl Atom for Variable {}

pub trait Language: std::cmp::PartialEq + std::clone::Clone {
    type Atom: Atom;
    type UnaryOpp: fmt::Display + FromStr + Eq + PartialEq + Clone;
    type BinaryOpp: fmt::Display + FromStr + Eq + PartialEq + Clone;
    type Function: fmt::Display + FromStr + Eq + PartialEq + Clone;
}

pub fn is_symbol<L: Language>(s: &str) -> bool {
    s == "(" ||
    s == ")" ||
    s == "," ||
    s.parse::<L::Atom>().is_ok() ||
    s.parse::<L::UnaryOpp>().is_ok() ||
    s.parse::<L::BinaryOpp>().is_ok() ||
    s.parse::<L::Function>().is_ok()
}

#[derive(PartialEq, Eq, Clone)]
pub enum Formula<L: Language> {
    Atom(L::Atom),
    UnaryOpp(L::UnaryOpp, Box<Formula<L>>),
    BinaryOpp(Box<Formula<L>>, L::BinaryOpp, Box<Formula<L>>),
    Function(L::Function, Vec<Formula<L>>),
}


pub struct FormulaIterator<'a, L: Language> {
    f: &'a Formula<L>,
    i: usize,
    nested: Option<Box<FormulaIterator<'a, L>>>,
}

impl<'a, L: Language> Iterator for FormulaIterator<'a, L> {
    type Item = &'a L::Atom;
    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.nested {
            Some(iter) => {
                let next = iter.next();
                if next.is_none() {
                    self.nested = None;
                }
                return next;
            },
            None => {},
        }
        match self.f {
            Formula::Atom(a) => {
                if self.i != 0 {
                    None
                } else {
                    self.i += 1;
                    Some(a)
                }
            },
            Formula::UnaryOpp(_, next) => {
                if self.i != 0 {
                    None
                } else {
                    let mut nested = Self {
                        f: next,
                        i: 0,
                        nested: None,
                    };
                    let r = nested.next();
                    if r.is_some() {
                        self.nested = Some(Box::new(nested));
                    }
                    r
                }
            }
            Formula::BinaryOpp(a, _, b) => {
                let r = if self.i == 0 {
                    a
                } else if self.i == 1 {
                    b
                } else {
                    return None;
                };
                let mut nested = Self {
                    f: r,
                    i: 0,
                    nested: None,
                };
                let r = nested.next();
                if r.is_some() {
                    self.nested = Some(Box::new(nested));
                }
                r
            }
            Formula::Function(_, args) => {
                if self.i < args.len() {
                    let mut nested = Self {
                        f: &args[self.i],
                        i: 0,
                        nested: None,
                    };
                    let r = nested.next();
                    if r.is_some() {
                        self.nested = Some(Box::new(nested));
                    }
                    r
                } else {
                    None
                }
            }
        }
    }
}


pub fn get_atoms<L: Language>(f: &Formula<L>) -> HashSet<&L::Atom> {
    match f {
        Formula::Atom(a) => HashSet::from([a]),
        Formula::UnaryOpp(_, a) => get_atoms(a),
        Formula::BinaryOpp(a, _, b) => {
            let mut set = get_atoms(a);
            get_atoms(b)
                .into_iter()
                .collect_into(&mut set);
            set
        },
        Formula::Function(_, args) => args.iter()
            .map(|arg| { get_atoms(arg) })
            .reduce(|mut a, b| {
                b.into_iter().collect_into(&mut a);
                a
            })
            .unwrap_or_default(),
    }
}

pub fn get_atoms_mut<L: Language>(f: &mut Formula<L>) -> Vec<&mut L::Atom> {
    match f {
        Formula::Atom(a) => Vec::from([&mut*a]),
        Formula::UnaryOpp(_, a) => get_atoms_mut(a),
        Formula::BinaryOpp(a, _, b) => {
            let mut set = get_atoms_mut(a);
            get_atoms_mut(b)
                .into_iter()
                .collect_into(&mut set);
            set
        },
        Formula::Function(_, args) => args.iter_mut()
            .map(|arg| { get_atoms_mut(arg) })
            .reduce(|mut a, b| {
                b.into_iter().collect_into(&mut a);
                a
            })
            .unwrap_or_default(),
    }
}

fn parse_from_symboles<'a, L: Language>(symbols: &'a [&'a str]) -> Result<(Formula<L>, usize), String> {
    if symbols.is_empty() {
        return Err(String::from("unexpected end of source"));
    }
    if symbols[0] == "(" {
        let (lhs, lhs_len) = parse_from_symboles::<L>(&symbols[1..])?;
        if symbols.len() <= lhs_len + 2 {
            return Err(String::from("unexpected end of source"));
        }
        if symbols[lhs_len + 1] == ")" {
            return Ok((lhs, lhs_len + 2));
        }
        let opp = symbols[lhs_len + 1].parse::<L::BinaryOpp>().map_err(|_| {
            format!("unexpected {}, BinaryOpp expected", symbols[lhs_len])
        })?;
        let (rhs, rhs_len) = parse_from_symboles::<L>(&symbols[(lhs_len + 2)..])?;
        if symbols.len() < lhs_len + rhs_len + 3 {
            return Err(String::from("unexpected end of source"));
        }
        if symbols[lhs_len + rhs_len + 2] != ")" {
            return Err(format!("unexpected {}, ')' expected", symbols[lhs_len + rhs_len + 2]));
        }
        Ok((Formula::BinaryOpp(Box::new(lhs), opp, Box::new(rhs)), lhs_len + rhs_len + 3))
    } else if let Ok(opp) = symbols[0].parse::<L::UnaryOpp>() {
        if symbols.len() < 2 {
            return Err(String::from("unexpected end of source"));
        }
        let (rhs, i) = parse_from_symboles(&symbols[1..])?;
        Ok((Formula::UnaryOpp(opp, Box::new(rhs)), i + 1))
    } else if let Ok(f) = symbols[0].parse::<L::Function>() {
        if symbols[1] != "(" {
            return Err(format!("unexpected {}, '(' expected after Function symbols", symbols[1]));
        }
        let mut i = 2;
        let mut args = Vec::new();
        while symbols[i] != ")" {
            let (arg, arg_len) = parse_from_symboles::<L>(&symbols[i..])?;
            args.push(arg);
            i += arg_len;
            if symbols[i] == "," {
                i += 1;
            } else if symbols[i] != ")" {
                return Err(format!("unexpected {}, ',' or ')' expected after argument", symbols[1]));
            }
        }
        Ok((Formula::Function(f, args), i + 1))
    } else if let Ok(a) = symbols[0].parse::<L::Atom>() {
        Ok((Formula::Atom(a), 1))
    } else {
        Err(format!("illegal expression {}", symbols[0]))
    }
}

impl<L: Language> FromStr for Formula<L> {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut symbols = Vec::new();
        let mut i = 0;
        while i < s.len() {
            let mut j = s.len();
            while j > i && !is_symbol::<L>(&s[i..j]) {
                j -= 1;
            }
            if j == i {
                i += 1;
            } else {
                symbols.push(&s[i..j]);
                i = j;
            }
        }

        parse_from_symboles(&symbols[..]).map(|(f, _)| {f})
    }
}

impl<L: Language> fmt::Display for Formula<L> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Atom(a) => a.fmt(f)?,
            Self::UnaryOpp(o, a) => {
                o.fmt(f)?;
                a.fmt(f)?;
            },
            Self::BinaryOpp(a, o, b) => {
                f.write_str("(")?;
                a.fmt(f)?;
                o.fmt(f)?;
                b.fmt(f)?;
                f.write_str(")")?;
            },
            Self::Function(o, m) => {
                o.fmt(f)?;
                f.write_str("(")?;
                if !m.is_empty() {
                    m[0].fmt(f)?;
                    if m.len() > 1 {
                        for a in &m[1..] {
                            f.write_str(", ")?;
                            a.fmt(f)?; 
                        }
                    }
                }

                f.write_str(")")?;
            },
        }
        fmt::Result::Ok(())
    }
}
