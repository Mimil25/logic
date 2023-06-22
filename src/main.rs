#![feature(iter_collect_into)]
#![feature(inherent_associated_types)]
mod formula;
mod interpretation;
mod opperators;
mod propositional_logic;
mod replacement;

use std::io::Write;

use formula::*;
use propositional_logic::PropositionalLanguage;
use propositional_logic::boolean_interpretation::{truth_table_print, to_cnf};

fn main() {
    loop {
        print!("Enter propositional statement : ");
        let mut s = String::new();
        std::io::stdout().flush().unwrap();
        std::io::stdin().read_line(&mut s).unwrap();
        let r = s.parse::<Formula<PropositionalLanguage<Variable>>>();
        match r {
            Ok(mut f) => {
                println!("{}", f);
                truth_table_print(&f).unwrap();
                to_cnf(&mut f);
                println!("{}", f);
                truth_table_print(&f).unwrap();
            },
            Err(e) => eprintln!("Error : {}", e),
        }
    }
}
