// I've gotten my signals crossed, apparently
// https://adventofcode.com/2021/day/8

use std::vec::Vec;
use itertools::Itertools;
use std::fs::File;
use std::io::{BufRead, BufReader};

// const FILENAME: &str = "one-line.txt";
// const FILENAME: &str = "example.txt";
const FILENAME: &str = "signals.txt";

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

fn is_substring(string: &String, sub: &String) -> bool {
    // returns true if the given 'sub' is entirely contained in 'string'
    for char_ in sub.chars() {
        if !string.contains(char_) {
            return false;
        }
    }
    // println!("String {} entirely contains {}", string, sub);
    return true;
}

// use known information to determine which unknowns are solveable
fn sort_known_from_unknown<'a>(knowns: &mut Vec<(&'a String, usize)>, unknowns: &mut Vec<(&'a String, Vec<usize>)>) {
    // use known information to filter unknowns
    for known in knowns.iter() {
        // walk through our known values, verifying that the overlap they have
        // with this unknown cell matches what we would expect from the correct
        // answers
        for unknown in unknowns.iter_mut() {
            let is_sub = is_substring(unknown.0, known.0);
            
            // verify that there either is or isn't overlap
            //  in the 'correct' digits as well
            let mut to_keep: Vec<usize> = Vec::new();
            for option in unknown.1.iter() {
                // println!("Checking option {}", option);
                let should_be_sub = is_substring(&CHAR_MAP[*option].to_string(), &CHAR_MAP[known.1].to_string());
                if should_be_sub == is_sub {
                    to_keep.push(*option);
                } else {
                    println!("Dropping {} as an option from {}", option, unknown.0);
                }
            }

            // update unknown options
            unknown.1 = to_keep;
        }
    }

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
}

fn string_diff(s1: &String, s2: &String) -> String {
    // initialize result
    let mut result = String::new();

    for c in s1.chars() {
        if !s2.contains(c) {
            result.push(c);
        }
    }
    for c in s2.chars() {
        if !s1.contains(c) {
            result.push(c);
        }
    }
    return result;
}

// use the given known information to extract mapping information
fn update_mapping(knowns: & Vec<(&String, usize)>, mapping: &mut Vec<(char,char)>) -> bool {
    // returns true if we have all the characters solved
    // the mapping represents our knowledge of which characters in our sequence
    // map to characters in the "correct" sequence
    
    // iterate through knowns and compare them to extract unique characters
    for known in knowns.iter() {
        // again iterate, only considering those that are 1 larger than us
        for other_known in knowns.iter() {
            let diff = string_diff(&known.0, &other_known.0);
            if diff.chars().count() == 1 {
                // consider just this char
                let char_: char = diff.chars().next().expect("This is a char");
                
                // check what the corresponding character is for the 'correct' data
                let correct_diff = string_diff(&CHAR_MAP[known.1].to_string(), &CHAR_MAP[other_known.1].to_string());
                let correct_char: char = correct_diff.chars().next().expect("This is a char.");

                let mut already_found = false;
                for kv in mapping.iter() {
                    if (kv.0 == char_) && (kv.1 == correct_char) {
                        already_found = true;
                    }
                }

                // append if this is new
                if !already_found {
                    println!("Found two strings with a diff of 1: {} - {} = {}", known.0, other_known.0, char_);
                    println!("This corresponds to ({} <-> {})", char_, correct_char);
                    mapping.push((char_, correct_char));
                }
            }
        }
    }

    // again go through knowns, this time removing known mappings from them and their corresponding
    // correct solution
    for known in knowns.iter() {
        let mut unmapped_chars = known.0.clone();
        let mut unmapped_correct_chars = CHAR_MAP[known.1].to_string();

        // filter out values that are already mapped
        for kv in mapping.iter() {
            unmapped_chars.retain(|c| { c != kv.0 });
            unmapped_correct_chars.retain(|c| { c != kv.1 });
        }

        // println!("After filtering: {} <-> {}", unmapped_chars, unmapped_correct_chars);
        assert_eq!(unmapped_chars.len(), unmapped_correct_chars.len());

        // we can add this to our mapping!
        if unmapped_chars.len() == 1 {
            let char_: char = unmapped_chars.chars().next().expect("This is a char.");
            let correct_char: char = unmapped_correct_chars.chars().next().expect("This is a char.");
            mapping.push((char_, correct_char));
            println!("Found mapping {} <-> {}", char_, correct_char);
        }
    }

    return mapping.len() == 7;
}

// iteratively process the given pattern until all elements are known
fn iterative_solve(patterns: & Vec<String>) -> Vec<(char,char)> {
    // initialize objects used to track our current understanding
    let mut knowns: Vec<(&String,usize)> = Vec::new();
    let mut unknowns: Vec<(&String,Vec<usize>)> = Vec::new();
    let mut mapping: Vec<(char,char)> = Vec::new();

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
    let mut iteration = 0;
    loop {
        sort_known_from_unknown(&mut knowns, &mut unknowns);

        if update_mapping(& knowns, &mut mapping) {
            println!("Found all characters, stopping.");
            break;
        }

        // make sure we made progress
        assert_ne!(knowns.len(), last_known_size);
        last_known_size = knowns.len();
        iteration += 1;

        // debug printouts
        println!("\tIter #{}", iteration); 
        println!("\t Known: {:?}", knowns);
        println!("\t Unknown: {:?}", unknowns);
        println!("\t Mapping: {:?}", mapping);
    }

    return mapping;
}

fn decode(output: & Vec<String>, mapping: & Vec<(char,char)>) -> u64 {
    // decode the given output strings (4 digits) via the given mapping
    
    let mut result = 0;
    let ten: u64 = 10;
    for (i, digit) in output.iter().enumerate() {
        // use mapping to get the correct sequence here
        let mut correct: String = String::new();
        for char_ in digit.chars() {
            for kv in mapping.iter() {
                if char_ == kv.0 {
                    correct.push(kv.1);
                }
            }
        }

        // println!("Converted {} to {}", digit, correct);
        // find corresponding number now - sorting first for convenience
        correct = correct.chars().sorted().collect::<String>();

        for (j, val) in CHAR_MAP.iter().enumerate() {
            if val.to_string() == correct {
                println!("Found expected digit: {} -> {}", digit, j);
                result += ten.pow(3 - i as u32) * (j as u64);
            }
        }
    }

    println!("Got {} for {:?}", result, output);
    return result;
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
    let mut result = 0;
    for i in 0..patterns.len() {
        // returns a mapping of wire->segment
        let mapping = iterative_solve(&patterns[i]);

        // decode output values
        result += decode(&outputs[i], &mapping);
    }

    println!("Part B: Got {}", result);
}
