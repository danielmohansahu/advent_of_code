// Scoring Syntax Sucks
// https://adventofcode.com/2021/day/10

use std::vec::Vec;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use anyhow::Result;

const FILENAME: &str = "example.txt";
// const FILENAME: &str = "navigation.txt";

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

fn score_line(line: & Vec<char>) -> Result<u32> {
    // score based on our scoring rules
    // incomplete line -> 0
    // illegal ),],},> -> 3,57,1197,25137
    
    // initialize the map used to count beginning / ending values
    let mut map_count: HashMap<char,i32> = HashMap::new();
    map_count.insert('(', 0);
    map_count.insert('[', 0);
    map_count.insert('{', 0);
    map_count.insert('<', 0);

    // iterate through the line, collecting the number of matches
    for char_ in line.iter() {
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

        // check if any of the characters are negative - that indicates
        // an invalid close
        for kv in &map_count {
            if *kv.1 < 0 {
                println!("Found invalid character {}", kv.0);
                match kv.0 {
                    '(' => return Ok(3),
                    '[' => return Ok(57),
                    '{' => return Ok(1197),
                    '<' => return Ok(25137),
                    _   => panic!("Unexpected map key.")
                }
            }
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
        score += score_line(&line).expect("Unable to score line.");
    }

    println!("Part A Syntax Score: {}", score);
}
