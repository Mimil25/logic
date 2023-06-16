use std::{fmt, str::FromStr};

trait Language {
    type Atom: fmt::Display + FromStr;
    type UnaryOpp: fmt::Display + FromStr;
    type BinaryOpp: fmt::Display + FromStr;
    type Function: fmt::Display + FromStr;

    fn is_symbol(s: &str) -> bool;
}

enum Formula<L: Language> {
    Atom(L::Atom),
    UnaryOpp(L::UnaryOpp, Box<Formula<L>>),
    BinaryOpp(Box<Formula<L>>, L::BinaryOpp, Box<Formula<L>>),
    Function(L::Function, Vec<Formula<L>>),
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
        if symbols.len() <= lhs_len + rhs_len + 3 {
            return Err(String::from("unexpected end of source"));
        }
        if symbols[lhs_len + rhs_len + 1] != ")" {
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

impl<L: Language> Formula<L> {
    fn parse(s: &str) -> Result<Self, String> {
        let mut symbols = Vec::new();
        let mut i = 0;
        while i < s.len() {
            let mut j = i + 1;
            while L::is_symbol(&s[i..j]) {
                j += 1;
            }
            j -= 1;
            if j == i {
                // not a symbol
            } else {
                symbols.push(&s[i..j]);
            }
            i = j;
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
