// simulate the exponential growth of lanternfish population
// https://adventofcode.com/2021/day/6

use std::vec::Vec;
use std::fs::File;
use std::io::{BufRead, BufReader};

// const FILENAME: &str = "example.txt";
const FILENAME: &str = "starting_fish.txt";
const NUM_DAYS: u32 = 80;

// parse the input file to get starting fish count
fn parse_input(filename: &str) -> Vec<u8> {

    // initialize result
    let mut result: Vec<u8> = Vec::new();

    // set up a reader and file object
    let file = File::open(filename).expect("Unable to open file.");
    let reader = BufReader::new(file);

    // there should only be one line
    let line = reader.lines().next().expect("Unable to read first line.").expect("Same");
    for age in line.split(',') {
        result.push(age.parse().expect("Got a non-digit input."));
    }

    return result;
}

fn main() {
    // get starting point
    let mut fishes = parse_input(FILENAME);

    // iterate for desired number of days, incrementing as necessary
    for i in 0..NUM_DAYS {
        let mut new_fishes: u32 = 0;
        for fish in fishes.iter_mut() {
            // handle fish aging
            if *fish == 0 {
                *fish = 6;
                new_fishes += 1;
            } else {
                *fish -= 1;
            }
        }

        // add new fishes
        for _ in 0..new_fishes {
            fishes.push(8);
        }

        // print fish, for debugging
        println!("Day {}", i+1);
        // println!("Day {}: {:?}", i+1, fishes);
    }

    println!("Final fish count: {}", fishes.len());
}
