// I've gotten my signals crossed, apparently
// https://adventofcode.com/2021/day/8

use std::vec::Vec;
use std::fs::File;
use std::io::{BufRead, BufReader};

// const FILENAME: &str = "example.txt";
const FILENAME: &str = "signals.txt";

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
    println!("Found {} outputs of type (1,4,7,8).", part_a_count);


}
