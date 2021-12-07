// organizing crabs, as one does
// https://adventofcode.com/2021/day/7

use std::vec::Vec;
use std::fs::File;
use std::io::{BufRead, BufReader};

// const FILENAME: &str = "example.txt";
const FILENAME: &str = "positions.txt";

// parse the input file to get starting positions
fn parse_input(filename: &str) -> Vec<u64> {

    // initialize result
    let mut result: Vec<u64> = Vec::new();

    // set up a reader and file object
    let file = File::open(filename).expect("Unable to open file.");
    let reader = BufReader::new(file);

    // there should only be one line
    let line = reader.lines().next().expect("Unable to read first line.").expect("Same");
    for age in line.split(',') {
        result.push(age.parse().expect("Got a non-digit input."))
    }

    return result;
}

fn main() {
    // get starting positions
    let mut positions = parse_input(FILENAME);

    // get the median
    positions.sort();
    let median = positions[positions.len() / 2];

    // calculate costs to get to median
    let mut cost = 0;
    for pos in &positions {
        cost += ((median as i64) - (*pos as i64)).abs();
    }

    println!("Optimum position is {} (costs {})", median, cost);
}
