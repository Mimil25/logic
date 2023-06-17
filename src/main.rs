#![feature(iter_collect_into)]
mod formula;
mod interpretation;
mod opperators;
mod propositional_logic;

use std::collections::HashMap;

use formula::*;
use propositional_logic::PropositionalLanguage;
use propositional_logic::boolean_interpretation::BooleanInterpretation;
use interpretation::*;

fn main() {
    let a: Formula<PropositionalLanguage<Variable>> = "((A && B) || !A)".parse().unwrap();
    println!("{}", a);
    let atoms = get_atoms(&a);
    println!("{:?}", &atoms);
    let vals: HashMapValuation<PropositionalLanguage<Variable>, bool> = HashMapValuation::try_from(HashMap::from([("A", true), ("B", false)])).unwrap();

    println!("{:?}", &vals);
    
    let result = evaluate::<
        PropositionalLanguage<Variable>,
        bool,
        BooleanInterpretation,
        HashMapValuation<PropositionalLanguage<Variable>, bool>
        >(&a, &vals);
    println!(" = {}", result)
}
