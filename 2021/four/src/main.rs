// play a game of bingo
// https://adventofcode.com/2021/day/4

use std::vec::Vec;
use std::fs::File;
use std::io::{BufRead, BufReader};

// const FILENAME : &str = "example.txt";
const FILENAME : &str= "cards.txt";

fn parse_cards(filename : &str, cards : &mut Vec<Vec<(u8,bool)>>, turns : &mut Vec<u8>) -> std::io::Result<()> {
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
                cards.last_mut().expect("Missing last element").push((element, false));
            }
        }
        // println!("Current line: {}", line);
    }

    Ok(())
}

fn mark_and_eval(card : &mut Vec<(u8,bool)>, turn : u8) -> bool {
    // return true if the given card is a winner after
    // the given turn order, false otherwise.
    // winner conditions: any column or row is all marked

    // iterate through the given card, marking any elements that match
    for tup in &mut card.iter_mut() {
        if tup.0 == turn {
            assert!(!tup.1);
            tup.1 = true;
            // I think we can break here? There shouldn't be any duplicates, right?
        }
    }

    // evaluate whether or not this card is a winner
    // I know. I'm ashamed of it too.
    card[0..5].iter().all(|&i| i.1 == true)
        || card[5..10].iter().all(|&i| i.1 == true) 
        || card[10..15].iter().all(|&i| i.1 == true) 
        || card[15..20].iter().all(|&i| i.1 == true) 
        || card[20..25].iter().all(|&i| i.1 == true) 
        || (card[0].1 && card[5].1 && card[10].1 && card[15].1 && card[20].1)
        || (card[1].1 && card[6].1 && card[11].1 && card[16].1 && card[21].1)
        || (card[2].1 && card[7].1 && card[12].1 && card[17].1 && card[22].1)
        || (card[3].1 && card[8].1 && card[13].1 && card[18].1 && card[23].1)
        || (card[4].1 && card[9].1 && card[14].1 && card[19].1 && card[24].1)
}

fn score(card : & Vec<(u8,bool)>, turn : u8) -> u32 {
    // convenience type conversion
    let turn = turn as u32;

    // calculate the score of the (winning, presumably) card
    let mut unmarked_sum : u32 = 0;
    for element in card {
        if !element.1 {
            unmarked_sum += element.0 as u32;
        }
    }

    println!("{} * {} = {}", unmarked_sum, turn, unmarked_sum * turn);
    unmarked_sum * turn
}

fn main() {
    // extract card information
    let mut cards : Vec<Vec<(u8,bool)>> = Vec::new();
    let mut turns : Vec<u8> = Vec::new();
    parse_cards(FILENAME, &mut cards, &mut turns).expect("Unable to parse input.");

    println!("Playing BINGO from input {}", FILENAME);

    // iterate through the turn order, starting from turn 5, until we get a winner
    let mut winner_found = false;
    for turn in turns {
        if !winner_found {
            for card in &mut cards {
                if mark_and_eval(card, turn) {
                    score(card, turn);
                    winner_found = true;
                    break;
                }
            }
        }
    }




}
