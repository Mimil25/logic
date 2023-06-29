#![feature(iter_collect_into)]
#![feature(inherent_associated_types)]
#![feature(generic_const_exprs)]
mod formula;
mod interpretation;
mod opperators;
mod propositional_logic;
mod replacement;
mod sudoku;

use std::io::Write;
use std::borrow::Cow;

use formula::*;
use propositional_logic::PropositionalLanguage;
use propositional_logic::boolean_interpretation::{truth_table_print, resolve};

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
                resolve(&mut f);
                println!("{}", f);
                truth_table_print(&f).unwrap();
            },
            Err(e) => eprintln!("Error : {}", e),
        }
    }
}
