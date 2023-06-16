use std::fmt;
use std::str::FromStr;

macro_rules! opp {
    ($name:ident, $symbol:literal) => {
        pub struct $name;

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str($symbol)
            }
        }

        impl FromStr for $name {
            type Err = ();
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                if s == $symbol {
                    Ok($name)
                } else {
                    Err(())
                }
            }
        }        
    };
}

macro_rules! group_opps {
    ($name:ident, $($opp:ident),+) => {
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
        
        impl FromStr for $name {
            type Err = ();
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                $(
                    if let Ok(o) = s.parse::<$opp>() {
                        Ok(Self::$opp(o))
                } else )+ {
                    Err(())
                }
            }
        }        
    };
}

pub(crate) use group_opps;

// propositional logic
opp!(Neg, "!");
opp!(And, "&&");
opp!(Or, "||");

// specials
opp!(NoOp, "");


