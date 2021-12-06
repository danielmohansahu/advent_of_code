// simulate the exponential growth of lanternfish population
// https://adventofcode.com/2021/day/6

use std::vec::Vec;
use std::fs::File;
use std::io::{BufRead, BufReader};

// const FILENAME: &str = "example.txt";
const FILENAME: &str = "starting_fish.txt";
const NUM_DAYS: u32 = 256;

// parse the input file to get starting fish count
fn parse_input(filename: &str) -> Vec<u64> {

    // initialize result
    let mut result: Vec<u64> = vec![0;9];

    // set up a reader and file object
    let file = File::open(filename).expect("Unable to open file.");
    let reader = BufReader::new(file);

    // there should only be one line
    let line = reader.lines().next().expect("Unable to read first line.").expect("Same");
    for age in line.split(',') {
        let age: usize = age.parse().expect("Got a non-digit input.");
        result[age] += 1;
    }

    return result;
}

fn main() {
    // get starting point - each index is the number of fish at that age
    let mut fish_ages = parse_input(FILENAME);

    // iterate for desired number of days, incrementing as necessary
    for i in 0..NUM_DAYS {
        // create next day's ages from existing
        let mut next_day: Vec<u64> = vec![0;9];
        for (age, count) in fish_ages.iter().enumerate() {
            // special handline for index 0 - pregnant fishes
            if age == 0 {
                next_day[8] = *count;
                next_day[6] = *count;
            } else {
                next_day[age - 1] += *count;
            }
        }

        // update ages
        fish_ages = next_day;

        // print fish, for debugging
        // println!("Day {}", i+1);
        println!("Day {}: {:?}", i+1, fish_ages);
    }

    let count: u64 = fish_ages.iter().sum();
    println!("Final fish count: {}", count);
}
