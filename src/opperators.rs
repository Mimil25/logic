use std::fmt;
use crate::formula::Opp;

macro_rules! opp {
    ($name:ident, $symbol:literal) => {
        #[derive(Debug, Eq, PartialEq, Hash, Clone)]
        pub struct $name;

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str($symbol)
            }
        }
        
        impl TryFrom<(&[&str], &mut usize)> for $name {
            type Error = String;
            fn try_from(value: (&[&str], &mut usize)) -> Result<Self, Self::Error> {
                let mut v_i = value.0.iter();
                let mut v_ii = v_i.next().unwrap().chars();
                let mut len = 1;
                for i in 0..$symbol.len() {
                    let r = v_ii.next();
                    let c;
                    if r.is_none() {
                        let rr = v_i.next();
                        len += 1;
                        if rr.is_none() {
                            return Err(format!("unexpected end while parsing {}", $symbol));
                        }
                        v_ii = rr.unwrap().chars();
                        c = v_ii.next().unwrap();
                    } else {
                        c = r.unwrap();
                    }
                    if $symbol[i..i+1].chars().next().unwrap() != c {
                        return Err(String::new());
                    }
                }
                *value.1 = len;
                Ok(Self)
            }
        }

        impl Opp for $name {}
    };
}

macro_rules! group_opps {
    ($name:ident, $($opp:ident),+) => {
        #[derive(Debug, Eq, PartialEq, Hash, Clone)]
        pub enum $name {
            $(
                $opp($opp),
            )+
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $(
                        Self::$opp(o) => o.fmt(f),
                    )+
                }
            }
        }
        
        impl TryFrom<(&[&str], &mut usize)> for $name {
            type Error = String;
            fn try_from(value: (&[&str], &mut usize)) -> Result<Self, Self::Error> {
                $(
                    if let Ok(o) = $opp::try_from((value.0, &mut*value.1)) {
                        Ok(Self::$opp(o))
                } else )+ {
                    Err(String::new())
                }
            }
        } 
        
        impl Opp for $name {}
    };
}

pub(crate) use group_opps;

// propositional logic
opp!(Neg, "!");
opp!(And, "&&");
opp!(Or, "||");
opp!(Implie, "=>");
opp!(IfAndOnlyIf, "<=>");

opp!(Conjuction, "ALL");
opp!(Disjuction, "ANY");

// specials
opp!(NoOp, "");


