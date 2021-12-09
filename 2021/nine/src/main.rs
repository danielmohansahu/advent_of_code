// Analyzing heightmaps to avoid asphyxiation
// https://adventofcode.com/2021/day/9

use std::vec::Vec;
use std::fs::File;
use std::io::{BufRead, BufReader};

// const FILENAME: &str = "example.txt";
const FILENAME: &str = "heightmap.txt";

// parse the input file to get heightmap
fn parse_input(filename: &str) -> Vec<Vec<u8>> {

    // initialize output
    let mut map: Vec<Vec<u8>> = Vec::new();

    // set up a reader and file object
    let file = File::open(filename).expect("Unable to open file.");
    let reader = BufReader::new(file);

    // parse all the lines into our various arrays
    for line in reader.lines() {
        let line = line.expect("Unable to read line.");

        // initialize new row
        let mut row: Vec<u8> = Vec::new();
        
        // parse each character into a new vector element
        for char_ in line.chars() {
            let digit: u8 = char_.to_digit(10).expect("Unable to parse into digit.") as u8;
            row.push(digit);
        }

        // push this to our result map
        map.push(row);
    }

    return map;
}

// find the values of all local minima
//  local minima are the locations which are lower then all their adjacent (non-diagonal) neighbors
fn local_minima(heightmap: & Vec<Vec<u8>>) -> Vec<u8> {
    // initialize result
    let mut minima: Vec<u8> = Vec::new();
    
    // iterate through the map, checking each element against its neighbors
    for i in 0..heightmap.len() {
        for j in 0..heightmap[i].len() {
            let e = heightmap[i][j];

            // compare against neighbors
            if (i != 0) && (e >= heightmap[i-1][j]) {
                // not lower than top
                continue;
            }
            if (i != heightmap.len() - 1) && (e >= heightmap[i+1][j]) {
                // not lower than bottom
                continue;
            }
            if (j != 0) && (e >= heightmap[i][j-1]) {
                // not lower than left
                continue;
            }
            if (j != heightmap[i].len() - 1) && (e >= heightmap[i][j+1]) {
                // not lower than right
                continue;
            }
            // if we've gotten this far we should be the local minima
            println!("Local minima detected at ({},{}): {}", i, j, e);
            minima.push(e);
        }
    }

    return minima;
}

fn main() {
    // parse input text file into heightmap
    let heightmap = parse_input(FILENAME);

    // for part A we want to detect the values of all local minima
    let mut risk_level: u64 = 0;
    for minimum in local_minima(&heightmap) {
        risk_level += (minimum as u64 + 1);
    }
    println!("Part A: Risk Level is {}", risk_level);
    



}
