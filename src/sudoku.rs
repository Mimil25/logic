use std::ops::Index;

const SIZE: usize = 3;

struct Sudoku {
    data: [Option<u32>; SIZE*SIZE*SIZE*SIZE],
}

fn coord_to_string(x: usize, y: usize, bit:usize) -> String {
    format!("x{}y{}b{}", x, y, bit).to_string()
}
/*
fn is_valid_sudoku() -> String {
    let max = SIZE*SIZE;
    let n_bits = (max as f32).log2().ceil() as usize;
    let num_valid = |x: usize, y: usize| -> String {
        fo
    }
}
*/
pub fn main() {
}
