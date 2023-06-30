use std::{fmt, str::FromStr};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Variable {
    name: String,
}

pub trait Atom:
    fmt::Debug +
    fmt::Display +
    Clone +
    PartialEq +
    Eq +
    for<'a> TryFrom<(&'a [&'a str], &'a mut usize), Error = String> +
    std::hash::Hash
{}

pub trait Opp:
    fmt::Debug +
    fmt::Display +
    Eq +
    PartialEq +
    std::hash::Hash +
    for<'a> TryFrom<(&'a [&'a str], &'a mut usize), Error = String> +
    Clone
{}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name.as_str())
    }
}

impl TryFrom<(&[&str], &mut usize)> for Variable {
    type Error = String;
    fn try_from(value: (&[&str], &mut usize)) -> Result<Self, Self::Error> {
        if value.0.len() >= 1 {
            if value.0[0].chars().all(|c| c.is_digit(36)) {
                *value.1 = 1;
                Ok(Variable { name: value.0[0].to_string()})
            } else {
                Err(format!("identifier {} should be alphanumeric", value.0[0]))
            }
        } else {
            Err(String::from("unexpected end when parsing Variable"))
        }
    }
}

impl Atom for Variable {}

pub trait Language: std::cmp::PartialEq + Eq + std::hash::Hash + std::clone::Clone + std::fmt::Debug {
    type Atom: Atom;
    type UnaryOpp: Opp;
    type BinaryOpp: Opp;
    type Function: Opp;
}

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub enum Formula<L: Language> {
    Atom(L::Atom),
    UnaryOpp(L::UnaryOpp, Box<Formula<L>>),
    BinaryOpp(Box<Formula<L>>, L::BinaryOpp, Box<Formula<L>>),
    Function(L::Function, Vec<Formula<L>>),
}


struct FormulaIterator<'a, L: Language> {
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
                    self.i += 1;
                    self.nested = None;
                } else {
                    return next;
                }
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
                    let nested = Self {
                        f: next,
                        i: 0,
                        nested: None,
                    };
                    self.nested = Some(Box::new(nested));
                    self.next()
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
                let nested = Self {
                    f: r,
                    i: 0,
                    nested: None,
                };
                self.nested = Some(Box::new(nested));
                self.next()
            }
            Formula::Function(_, args) => {
                if self.i < args.len() {
                    let nested = Self {
                        f: &args[self.i],
                        i: 0,
                        nested: None,
                    };
                    self.nested = Some(Box::new(nested));
                    self.next()
                } else {
                    None
                }
            }
        }
    }
}

impl<L: Language> Formula<L> {
    pub fn iter(&self) -> impl Iterator<Item = &L::Atom> {
        FormulaIterator {
            f: self,
            i: 0,
            nested: None,
        }
    }
    
    pub fn get_atoms_mut(&mut self) -> Vec<&mut L::Atom> {
        match self {
            Formula::Atom(a) => Vec::from([&mut*a]),
            Formula::UnaryOpp(_, a) => a.get_atoms_mut(),
            Formula::BinaryOpp(a, _, b) => {
                let mut set = a.get_atoms_mut();
                b.get_atoms_mut()
                    .into_iter()
                    .collect_into(&mut set);
                set
            },
            Formula::Function(_, args) => args.iter_mut()
                .map(|arg| { arg.get_atoms_mut() })
                .reduce(|mut a, b| {
                    b.into_iter().collect_into(&mut a);
                    a
                })
                .unwrap_or_default(),
        }
    }
}

impl<L: Language> TryFrom<(&[&str], &mut usize)> for Formula<L> {
    type Error = String;
    fn try_from(value: (&[&str], &mut usize)) -> Result<Self, Self::Error> {
        match value.0 {
            ["(", rest@..] => {
                let mut len_f = 0;
                let first = Formula::try_from((rest, &mut len_f))?;
                if rest.len() <= len_f {
                    return Err(String::from("unexpected end of symbols"));
                }
                if rest[len_f] == ")" {
                    *value.1 = 2 + len_f;
                    return Ok(first);
                }
                let mut len_o = 0;
                let opp = L::BinaryOpp::try_from((&rest[len_f..], &mut len_o))?;
                let mut len_s = 0;
                let second = Formula::try_from((&rest[(len_f+len_o)..], &mut len_s))?;
                let len = len_f + len_o + len_s;
                if rest.len() <= len {
                    return Err(String::from("unexpected end of symbols"));
                }
                if rest[len] != ")" {
                    return Err(format!("unexpected {}, ')' expected", rest[len]));
                }
                *value.1 = len + 2;
                Ok(Formula::BinaryOpp(Box::new(first), opp, Box::new(second)))
            },
            [rest@..] => {
                let mut len = 0;
                if let Ok(func) = L::Function::try_from((rest, &mut len)) { // maybe its a function
                    if len >= rest.len() {
                        return Err(String::from("unexpected end of symbols"));
                    }
                    if rest[len] == "(" { // its a function
                        let mut args = Vec::new();
                        len += 1;
                        if rest[len] == ")" {
                            return Ok(Formula::Function(func, args));
                        }
                        loop {
                            let mut len_arg = 0;
                            args.push(Formula::try_from((&rest[len..], &mut len_arg))?);
                            len += len_arg;
                            if rest[len] == ")" {
                                *value.1 = len + 1;
                                return Ok(Formula::Function(func, args));
                            } else if rest[len] != "," {
                                return Err(format!("unexpected {}, ',' or ')' expected", rest[len]));
                            }
                            len += 1;
                        }
                    } else { 
                        return Err(format!("unexpected {}, ',' or ')' expected", rest[len]));
                    }
                } else if let Ok(opp) = L::UnaryOpp::try_from((rest, &mut len)) { // maybe its an UnaryOpp
                    if len >= rest.len() {
                        return Err(String::from("unexpected end of symbols"));
                    }
                    let mut len_arg = 0;
                    let arg = Formula::try_from((&rest[len..], &mut len_arg))?;
                    *value.1 = len + len_arg;
                    return Ok(Formula::UnaryOpp(opp, Box::new(arg)));
                } else { // it can only be an atom
                    let atom = L::Atom::try_from(value)?;
                    Ok(Formula::Atom(atom))
                }
            },
        }
    }
}

impl<L: Language> FromStr for Formula<L> {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut symbols = Vec::new();
        let mut i = 0;
        
        while i < s.len() {
            let c = s.as_bytes()[i] as char;
            if !c.is_digit(36) && !c.is_whitespace() {
                symbols.push(&s[i..=i]);
            }
            let mut j = i;
            while j < s.len() && (s.as_bytes()[j] as char).is_digit(36) {
                j += 1;
            }
            if j != i {
                symbols.push(&s[i..j]);
                i = j;
            } else {
                i += 1;
            }
        }
        let mut _len = 0;
        Self::try_from((&symbols[..], &mut _len))
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
