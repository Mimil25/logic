mod formula;
mod opperators;
mod propositional_logic;

use formula::*;
use propositional_logic::PropositionalLanguage;

fn main() {
    let a: Formula<PropositionalLanguage<Variable>> = "((VARA && VARB) || !A)".parse().unwrap();
    println!("{}", a);
}
