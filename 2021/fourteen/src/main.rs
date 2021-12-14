// Polymerization at 10000 Feet Below
// https://adventofcode.com/2021/day/14

use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use anyhow::Result;

// const FILENAME: &str = "example.txt";
const FILENAME: &str = "rules.txt";

// parse the input file to get the starting paper and fold instructions
fn parse_input(filename: &str) -> Result<(HashMap<String,u64>,HashMap<char,u64>,HashMap<String,char>)> {

    // initialize outputs
    let mut rules: HashMap<String,char> = HashMap::new();
    let mut pair_counter: HashMap<String,u64> = HashMap::new();
    let mut char_counter: HashMap<char,u64> = HashMap::new();
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

    // get initial count of pairs
    let mut last: char = ' ';
    for (i,char_) in string.chars().enumerate() {
        if i != 0 {
            let mut tmp = last.to_string();
            tmp.push(char_);
            *pair_counter.entry(tmp).or_insert(0) += 1;
        }
        last = char_;
        *char_counter.entry(char_).or_insert(0) += 1;
    }

    Ok((pair_counter,char_counter,rules))
}

fn polymerize(pairs: &mut HashMap<String,u64>, counter: &mut HashMap<char,u64>, rules: &HashMap<String,char>) {
    // create local copy of pairs for iteration
    let pair_init = pairs.clone();

    // iterate through the existing string, updating pairs and counter
    let mut last: char = ' ';
    for (i,kv) in pair_init.iter().enumerate() {
        let key = kv.0;
        let count = kv.1;
        let c1 = key.chars().next().unwrap();
        let c2 = key.chars().last().unwrap();

        // create string and find rules
        let val = rules[key];
        // println!("Found key -> val {:?} -> {}", key,val);

        // append the two resulting strings to our map
        let (mut s1, mut s2): (String,String) = (c1.to_string(), val.to_string());
        s1.push(val);
        s2.push(c2);
        // println!("Made new pairs: {:?},{:?}", s1, s2);

        // update our counts
        *pairs.entry(s1).or_insert(0) += count;
        *pairs.entry(s2).or_insert(0) += count;
        // this or insert here is silly, but it will cause a panic if undefined (which is good)
        *pairs.entry(key.to_string()).or_insert(0) -= count;

        // also count how many times we've seen this char
        *counter.entry(val).or_insert(0) += count;
    }
}

fn count(counter: &HashMap<char,u64>) -> u64 {
    // returns the difference between most and least element occurence
    let (mut least, mut most): (u64, u64) = (u64::MAX, 0);
    for kv in counter {
        if *kv.1 > most {
            most = *kv.1;
        }
        if *kv.1 < least {
            least = *kv.1;
        }
    }

    return most - least;
}

fn main() {
    // parse input into starting string and rules
    let (mut pairs, mut counter, rules) = parse_input(FILENAME).unwrap();

    // iteratively apply rules
    println!("Step {}: {:?}", 0, pairs);
    for i in 0..40 {
        polymerize(&mut pairs, &mut counter, &rules);

        if i == 9 {
            // count for part A
            println!("Part A: Found a count difference of {}", count(&counter));
        }
        // println!("Step {}: {:?}", i + 1, pairs);
        println!("Step {}", i + 1);
    }

    println!("Part B: Found a count difference of {}", count(&counter));
    
    
}
