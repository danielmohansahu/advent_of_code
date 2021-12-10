// Scoring Syntax Sucks
// https://adventofcode.com/2021/day/10

use std::vec::Vec;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use anyhow::Result;

// const FILENAME: &str = "example.txt";
const FILENAME: &str = "navigation.txt";

// parse the input file to get each line of input
fn parse_input(filename: &str) -> Result<Vec<Vec<char>>> {

    // initialize output
    let mut input: Vec<Vec<char>> = Vec::new();

    // set up a reader and file object
    let file = File::open(filename)?;
    let reader = BufReader::new(file);

    // parse all the lines into our various arrays
    for line in reader.lines() {
        let line = line?;

        // initialize new row
        let mut row: Vec<char> = Vec::new();
        
        // parse each character into a new vector element
        for char_ in line.chars() {
            row.push(char_);
        }

        // push this to our result map
        input.push(row);
    }

    Ok(input)
}

// convenience function to track the scores of each character
fn score_closer(closer: char) -> u32 {
    match closer {
        ')' => return 3,
        ']' => return 57,
        '}' => return 1197,
        '>' => return 25137,
        _   => panic!("Unexpected character received: {}", closer)
    }
}

fn is_valid(slice: &[char]) -> bool {
    // return true if the given slice is valid, false otherwise
    // valid means all opening characters have an appropriate closing
    // note - this does not check their order! just raw count
    let mut map_count: HashMap<char,i32> = HashMap::new();
    map_count.insert('(', 0);
    map_count.insert('[', 0);
    map_count.insert('{', 0);
    map_count.insert('<', 0);
    for char_ in slice {
        match char_ {
            // opening characters
            '(' => *map_count.get_mut(&'(').unwrap() += 1,
            '[' => *map_count.get_mut(&'[').unwrap() += 1,
            '{' => *map_count.get_mut(&'{').unwrap() += 1,
            '<' => *map_count.get_mut(&'<').unwrap() += 1,
            // closing characters
            ')' => *map_count.get_mut(&'(').unwrap() -= 1,
            ']' => *map_count.get_mut(&'[').unwrap() -= 1,
            '}' => *map_count.get_mut(&'{').unwrap() -= 1,
            '>' => *map_count.get_mut(&'<').unwrap() -= 1,
            // error handling
            _   => panic!("Unexpected character received: {}", char_)
        }
    }

    // check if any of the characters are negative - that indicates
    // an invalid close
    for kv in &map_count {
        if *kv.1 != 0 {
            // println!("{:?} is invalid", slice);
            return false;
        }
    }
    // if we get here the slice is valid
    // println!("{:?} is valid", slice);
    true
}

fn score_line(line: & Vec<char>) -> Result<u32> {
    // score based on our scoring rules
    // incomplete line -> 0
    // illegal ),],},> -> 3,57,1197,25137

    // iterate through the line, collecting the number of matches
    for (i, char_) in line.iter().enumerate() {
        // check if this is a end character
        if ![')', ']', '}', '>'].contains(char_) {
            continue;
        }

        // walk backwards to find the corresponding open element
        let opener = match char_ {
            ')' => '(',
            ']' => '[',
            '}' => '{',
            '>' => '<',
            _   => panic!("Unexpected character received: {}", char_)
        };

        // println!("Searching backwards for corresponding '{}' for '{}' at index {}", opener, char_, i);
        let mut close_count = 1;
        for j in (0..i).rev() {
            if line[j] == *char_ {
                // we found another closing element, need to skip this subsegment
                close_count += 1;
            } else if line[j] == opener {
                close_count -= 1;
                // check if we're down to zero - this indicates we found our opener
                if (close_count == 0) && !is_valid(&line[j+1..i]) {
                    // println!("Found corresponding opener '{}' for '{}' at index {}", opener, char_, j);
                    return Ok(score_closer(*char_));
                } else if close_count == 0 {
                    // stop condition - we're happy with this close
                    break;
                }
            }
        }

        // if we never found a corresponding opener then this is also invalid
        if close_count != 0 {
            return Ok(score_closer(*char_));
        }
    }

    // if we got this far the line is either incomplete or valid
    return Ok(0);
}

fn main() {
    // collect input
    let input = parse_input(FILENAME).expect("Unable to parse input file.");
    
    // collect score
    let mut score: u32 = 0;
    for line in input {
        let sub_score = score_line(&line).expect("Unable to score line.");
        // println!("{:?} -> {}", line, sub_score);
        score += sub_score;
    }

    println!("Part A Syntax Score: {}", score);
}
