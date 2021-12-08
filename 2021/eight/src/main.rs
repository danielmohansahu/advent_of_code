// I've gotten my signals crossed, apparently
// https://adventofcode.com/2021/day/8

use std::vec::Vec;
use std::fs::File;
use std::io::{BufRead, BufReader};

const FILENAME: &str = "example.txt";
// const FILENAME: &str = "signals.txt";

// some constants used in Part2
const CHAR_MAP: [&'static str; 10] = ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"];

// parse the input file to get starting positions
fn parse_input(filename: &str, patterns: &mut Vec<Vec<String>>, outputs: &mut Vec<Vec<String>>) {

    // set up a reader and file object
    let file = File::open(filename).expect("Unable to open file.");
    let reader = BufReader::new(file);

    // parse all the lines into our various arrays
    for line in reader.lines() {
        let line = line.expect("Unable to read line.");
        
        // split by input/ouput delimiter |
        let mut split = line.split('|');

        // split again on whitespace for the inputs
        let mut signals: Vec<String> = Vec::new();
        for signal in split.next().expect("Missing input elements.").split_whitespace() {
            signals.push(signal.to_string());
        }

        // do the same for the results
        let mut results: Vec<String> = Vec::new();
        for result in split.next().expect("Missing input elements.").split_whitespace() {
            results.push(result.to_string());
        }

        // push these back to our main vectors
        patterns.push(signals);
        outputs.push(results);
    }
}

// use known information to determine which unknowns are solveable
fn sort_known_from_unknown<'a>(knowns: &mut Vec<(&'a String, usize)>, unknowns: &mut Vec<(&'a String, Vec<usize>)>) {
    // use known information to filter unknowns
    // @TODO!

    // check if we have any known values already (unknowns of len==1)
    for kv in unknowns.iter() {
        assert_ne!(kv.1.len(), 0);
        if kv.1.len() == 1 {
            // only one possibility
            knowns.push((kv.0, kv.1[0]));
        }
    }
    // remove knowns from unknowns
    unknowns.retain(|kv| {kv.1.len() != 1});

    // sanity checks
    assert_eq!(knowns.len() + unknowns.len(), 10);

    // debug printouts
    println!("\t Known: {:?}", knowns);
    println!("\t Unknown: {:?}", unknowns);
}

// iteratively process the given pattern until all elements are known
fn iterative_solve(patterns: & Vec<String>) {
    // initialize objects used to track our current understanding
    let mut knowns: Vec<(&String,usize)> = Vec::new();
    let mut unknowns: Vec<(&String,Vec<usize>)> = Vec::new();

    // initialize all values as unknown
    for pattern in patterns.iter() {
        // iterate through known lengths
        unknowns.push((pattern, Vec::new()));
        for (digit, chars) in CHAR_MAP.iter().enumerate() {
            if pattern.len() == chars.len() {
                unknowns.last_mut().expect("Empty vector").1.push(digit);
            }
        }
    }

    // perform initial filtering
    let mut last_known_size = 0;
    loop {
        // break conditions
        if unknowns.len() == 0 {
            break;
        }

        sort_known_from_unknown(&mut knowns, &mut unknowns);

        // make sure we made progress
        assert_ne!(knowns.len(), last_known_size);
        last_known_size = knowns.len();
    }

    // print results
    println!("Initial Pattern: {:?}", patterns);
    println!("\t Known: {:?}", knowns);
    println!("\t Unknown: {:?}", unknowns);


}

fn main() {
    // parse input files
    let (mut patterns, mut outputs): (Vec<Vec<String>>, Vec<Vec<String>>) = (Vec::new(), Vec::new());
    parse_input(FILENAME, &mut patterns, &mut outputs);

    // for part #1 we just want to count the number of outputs of type (1,4,7,8)
    //  we know these from their lengths: (2,4,3,7)
    let mut part_a_count = 0;
    let counting_sizes: Vec<usize> = vec![2,4,3,7];
    for results in outputs.iter() {
        // results is the outputs given in a single line, i.e. a single display
        for result in results.iter() {
            if counting_sizes.contains(&result.len()) {
                part_a_count += 1;
            }
        }
    }

    println!("Part A: found {} outputs of type (1,4,7,8).", part_a_count);

    // part 2 is a lot more complicated - we need to iteratively figure out which wire maps to
    // which segment.
    for i in 0..patterns.len() {
        // returns a mapping of wire->segment
        iterative_solve(&patterns[i]);

    }

}
