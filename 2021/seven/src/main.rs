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

fn calculate_cost(positions: &[u64], dest: u64) -> u64 {
    // returns the cost it would take to move to the given destination

    let mut cost = 0;
    for pos in positions {
        let steps = ((dest as i64) - (*pos as i64)).abs() as u64;
        for step in 1..(steps+1) {
            cost += step;
        }
    }

    cost
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

    println!("Optimum position for Part A is {} (costs {})", median, cost);

    // for part B we brute force things
    let mut min_cost = u64::MAX;
    let mut target = 0;
    for tgt in positions[0]..(positions[positions.len()-1]+1) {
        let cost = calculate_cost(&positions, tgt);
        if cost < min_cost {
            min_cost = cost;
            target = tgt;
        }
    }

    println!("Optimum position for Part B is {} (costs {})", target, min_cost);
}
