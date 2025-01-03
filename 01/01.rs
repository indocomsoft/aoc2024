use std::collections::HashMap;
use std::fs;

fn main() {
    let contents = fs::read_to_string("input").expect("failed to read file 'input'");
    let mut first_table: Vec<u32> = vec![];
    let mut second_table: Vec<u32> = vec![];
    for line in contents.trim_end().split('\n') {
        let (a, b) = line.split_once("   ").expect("unexpected line shape");
        first_table.push(a.parse().expect("unable to read first number"));
        second_table.push(b.parse().expect("unable to read second number"));
    }
    first_table.sort();
    second_table.sort();
    // Part 1
    let sum = first_table.iter().zip(second_table.iter()).map(|(a, b)| a.abs_diff(*b) as u64).reduce(|acc, e| acc + e).unwrap();
    println!("{}", sum);

    // Part 2
    let mut frequencies = HashMap::new();
    for b in second_table {
        frequencies.entry(b).and_modify(|e| *e += 1).or_insert(1);
    }
    let result = first_table.iter().map(|a| a * frequencies.get(a).unwrap_or(&0)).reduce(|acc, e| acc + e).unwrap();
    println!("{}", result);
}
