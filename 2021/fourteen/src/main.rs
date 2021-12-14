// Polymerization at 10000 Feet Below
// https://adventofcode.com/2021/day/14

use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use anyhow::Result;

// const FILENAME: &str = "example.txt";
const FILENAME: &str = "rules.txt";

// parse the input file to get the starting paper and fold instructions
fn parse_input(filename: &str) -> Result<(String,HashMap<String,char>)> {

    // initialize outputs
    let mut rules: HashMap<String,char> = HashMap::new();
    let mut string: String = String::new();

    // set up a reader and file object
    let file = File::open(filename)?;
    let reader = BufReader::new(file);

    // parse all the lines into our various arrays
    for (i,line) in reader.lines().enumerate() {
        let line = line?;

        // handle starting line
        if i == 0 {
            string = line;
            continue;
        } else if i == 1 {
            continue;
        }

        // add this new rule
        // println!("Processing rule: {:?}", line);
        let mut split = line.split(" -> ");
        let key = split.next().unwrap();
        let val = split.next().unwrap().chars().nth(0).unwrap();
        rules.insert(key.to_string(),val);
    }

    Ok((string.to_string(),rules))
}

fn polymerize(polymer: &String, rules: &HashMap<String,char>) -> String{
    // initialize result
    let mut result: String = String::new();

    // iterate through the existing string, adding new rules to the new one
    let mut last: char = ' ';
    #[allow(unused_assignments)]
    let mut key: String = String::new();
    for (i,char_) in polymer.chars().enumerate() {
        if i != 0 {
            // create string and find rules
            key = last.to_string();
            key.push(char_);

            // append this string to our result
            result.push(rules[&key]);
            result.push(char_);
            // println!("\t{:?}",result);
        } else {
            result.push(char_);
        }

        // update last element
        last = char_;
    }

    return result;
}

fn count(polymer: &String) -> u32 {
    // returns the difference between most and least element occurence
    let mut counter: HashMap<char,u32> = HashMap::new();
    for char_ in polymer.chars() {
        *counter.entry(char_).or_insert(0) += 1;
    }

    let (mut least, mut most): (u32, u32) = (u32::MAX, 0);
    for kv in counter {
        if kv.1 > most {
            most = kv.1;
        }
        if kv.1 < least {
            least = kv.1;
        }
    }

    return most - least;
}

fn main() {
    // parse input into starting string and rules
    let (mut current, rules) = parse_input(FILENAME).unwrap();

    // iteratively apply rules
    println!("Polymer at step {}: {:?}", 0, current);
    for i in 0..10 {
        current = polymerize(&current, &rules);
        // println!("Polymer at step {}: {:?}", i + 1, current);
    }

    // count for part A
    println!("Part A: Found a count difference of {}", count(&current));
    
    
}
