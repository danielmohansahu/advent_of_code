// play a game of bingo
// https://adventofcode.com/2021/day/4

use std::vec::Vec;
use std::fs::File;
use std::io::{BufRead, BufReader};

// const FILENAME : &str = "example.txt";
const FILENAME : &str= "cards.txt";

fn parse_cards(filename : &str, cards : &mut Vec<Vec<u8>>, turns : &mut Vec<u8>) -> std::io::Result<()> {
    // extract the turn order and set of cards from the given file

    // set up a reader and file object
    let file = File::open(filename)?;
    let reader = BufReader::new(file);

    // successive lines should be card values, interspersed by whitespace
    for (index, line) in reader.lines().enumerate() {
        // read the current line
        let mut line = line?;

        // the first line should be the turn order
        if index == 0 {
            // strip whitespace first
            line.retain(|c| !c.is_whitespace());
            for turn in line.split(",") {
                turns.push(turn.parse().expect("Got non-digit input."));
            }
            continue;
        }

        // iterate through the remaining lines (cards) and extract their information
        if line.chars().count() == 0 {
            // we've reached the start of a new card - initialize it
            assert!(cards.len() == 0 || cards.last().expect("This should exist").len() == 25);
            cards.push(Vec::new());
            continue;
        } else {
            // otherwise we want to append this value to our previous card
            for element in line.split_whitespace() {
                let element : u8 = element.parse().expect("Got non-digit input.");
                cards.last_mut().expect("Missing last element").push(element);
            }
        }
        // println!("Current line: {}", line);
    }

    Ok(())
}

fn main() {
    // extract card information
    let mut cards : Vec<Vec<u8>> = Vec::new();
    let mut turns : Vec<u8> = Vec::new();
    parse_cards(FILENAME, &mut cards, &mut turns).expect("Unable to parse input.");

    println!("Playing BINGO from input {}", FILENAME);




}
