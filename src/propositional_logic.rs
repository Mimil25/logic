use std::{fmt, str::FromStr, marker::PhantomData};

use crate::formula::*;
use crate::opperators::*;

pub struct PropositionalLanguage<A: fmt::Display + FromStr> {
    _phantom_data: PhantomData<A>
}

group_opps!(PLBinOpps, And, Or);

impl<A: fmt::Display + FromStr> Language for PropositionalLanguage<A> {
    type Atom = A;
    type UnaryOpp = Neg;
    type BinaryOpp = PLBinOpps;
    type Function = NoOp;
}
