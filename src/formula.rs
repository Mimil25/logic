use std::{fmt, str::FromStr, collections::HashSet};

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Variable {
    name: String,
}

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

pub trait Language {
    type Atom: fmt::Display + FromStr;
    type UnaryOpp: fmt::Display + FromStr;
    type BinaryOpp: fmt::Display + FromStr;
    type Function: fmt::Display + FromStr;
}

pub fn is_symbol<L: Language>(s: &str) -> bool {
    s == "(" ||
    s == ")" ||
    s.parse::<L::Atom>().is_ok() ||
    s.parse::<L::UnaryOpp>().is_ok() ||
    s.parse::<L::BinaryOpp>().is_ok() ||
    s.parse::<L::Function>().is_ok()
}

pub enum Formula<L: Language> {
    Atom(L::Atom),
    UnaryOpp(L::UnaryOpp, Box<Formula<L>>),
    BinaryOpp(Box<Formula<L>>, L::BinaryOpp, Box<Formula<L>>),
    Function(L::Function, Vec<Formula<L>>),
}

pub fn get_atoms<L: Language>(f: &Formula<L>) -> HashSet<&L::Atom> where L::Atom: Eq + std::hash::Hash {
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
