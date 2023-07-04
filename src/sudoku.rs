use std::fmt::format;

const SIZE: usize = 2;

struct Sudoku {
    data: [Option<u32>; SIZE*SIZE*SIZE*SIZE],
}

fn coord_to_string(x: usize, y: usize, bit: usize) -> String {
    format!("x{}y{}b{}", x, y, bit).to_string()
}

fn int_bit(n: usize, bit: usize) -> String {
    if (n >> bit) & 1 == 1 {
        "TRUE".to_string()
    } else {
        "FALSE".to_string()
    }
}

pub fn num_valid(x: usize, y: usize) -> String {
    let max = SIZE*SIZE;
    let n_bits = (max as f32).log2().ceil() as usize;
    let mut res = String::from("ANY(");
    for i in (0..n_bits).rev() {
        res += format!(
            "ALL((!{} && {})",
            coord_to_string(x, y, i).as_str(),
            int_bit(max, i).as_str()).as_str();
        for j in (i+1)..n_bits {
            res += format!(
                ",({} <=> {})",
                coord_to_string(x, y, j).as_str(),
                int_bit(max, j).as_str()).as_str();
        }
        res += "),";
    }
    res += "FALSE)";
    res
}

fn num_diff(x_a: usize, y_a: usize, x_b: usize, y_b: usize) -> String {
    let max = SIZE*SIZE;
    let n_bits = (max as f32).log2().ceil() as usize;
    let mut res = String::from("!ALL(");
    for i in 0..n_bits {
        res += format!(
            "({} <=> {}),",
            coord_to_string(x_a, y_a, i).as_str(),
            coord_to_string(x_b, y_b, i).as_str()
            ).as_str();
    }
    res += "TRUE)";
    res
}

fn num_equal(x: usize, y: usize, n: u32) -> String {
    let max = SIZE*SIZE;
    let n_bits = (max as f32).log2().ceil() as usize;
    let mut res = String::from("ALL(");
    for i in 0..n_bits {
        res += format!(
            "({} <=> {}),",
            coord_to_string(x, y, i).as_str(),
            int_bit(n as usize, i).as_str()
            ).as_str();
    }
    res += "TRUE)";
    res
}

pub fn is_valid_sudoku() -> String {
    let max = SIZE*SIZE;
    let mut res = String::from("ALL(");
    // all nums are valid
    /*
    for x in 0..max {
        for y in  0..max {
            res += num_valid(x, y).as_str();
            res += ",";
        }
    }
    */
    for x in 0..max {
        for y_a in 0..(max-1) {
            for y_b in (y_a+1)..max {
                res += num_diff(x, y_a, x, y_b).as_str();
                res += ",";
            }
        }
    }
    for y in 0..max {
        for x_a in 0..(max-1) {
            for x_b in (x_a+1)..max {
                res += num_diff(x_a, y, x_b, y).as_str();
                res += ",";
            }
        }
    }
    for x_block in 0..SIZE {
        for y_block in 0..SIZE {
            for x_a in 0..SIZE {
                for y_a in 0..SIZE {
                    for x_b in 0..SIZE {
                        for y_b in 0..SIZE {
                            if x_a != x_b || y_a != y_b {
                                res += num_diff(
                                    x_a + x_block,
                                    y_a + y_block,
                                    x_b + x_block,
                                    y_b + y_block).as_str();
                                res += ",";
                            }
                        }
                    }
                }
            }
        }
    }
    res += "TRUE)";
    res
}

pub fn sudoku_2x2_exemple() -> String {
    let mut res = String::from("ALL(");
    res += is_valid_sudoku().as_str();
    res += ",";
    res += num_equal(0, 0, 3).as_str();
    res += ",";
    res += num_equal(1, 0, 1).as_str();
    res += ",";
    res += num_equal(2, 0, 0).as_str();
    res += ",";
    res += num_equal(3, 0, 2).as_str();
    res += ",";
    res += num_equal(0, 2, 1).as_str();
    res += ",";
    res += num_equal(0, 3, 2).as_str();
    res += ",";
    res += num_equal(1, 3, 0).as_str();
    res += ",";
    res += num_equal(2, 3, 3).as_str();
    res += ")";
    res
}

pub fn main() {
    println!("{}", is_valid_sudoku());
}
