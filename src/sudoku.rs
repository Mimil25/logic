const SIZE: usize = 3;

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

fn num_valid(x: usize, y: usize) -> String {
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

pub fn is_valid_sudoku() -> String {
    let max = SIZE*SIZE;
    let mut res = String::from("ALL(");
    for x in 0..max {
        for y in  0..max {
            res += num_valid(x, y).as_str();
            res += ",";
        }
    }
    res += "TRUE )";
    res
}

pub fn main() {
    println!("{}", is_valid_sudoku());
}
