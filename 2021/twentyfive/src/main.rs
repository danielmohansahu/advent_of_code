// Tracking Sea Cucumber Patterns
// https://adventofcode.com/2021/day/25

use std::vec::Vec;
use anyhow::Result;
use std::fs::File;
use std::io::{BufRead, BufReader};

// static FILENAME: &str = "example.txt";
static FILENAME: &str = "cukes.txt";

// get the grid state from the given file
fn parse_input(filename: &str) -> Result<Vec<Vec<char>>> {
    // initialize result grid
    let mut grid = Vec::new();

    // set up a reader and file object
    let file = File::open(filename)?;
    let reader = BufReader::new(file);

    // parse inputs
    for line in reader.lines() {
        let line = line?;
        let mut row = Vec::new();

        for char_ in line.chars() {
            row.push(char_);
        }
        grid.push(row);
    }
    Ok(grid)
}

// convenience function to plot the current grid state
fn plot(grid: &Vec<Vec<char>>) {
    for i in 0..grid.len() {
        let mut row = String::new();
        for j in 0..grid[i].len() {
            row.push(grid[i][j]);
        }
        println!("  {:?}", row);
    }
}

// perform one iteration of the grid
fn step(grid: &Vec<Vec<char>>) -> Option<Vec<Vec<char>>> {
    // operate on a copy of the grid, only
    // returning it if something moved
    let mut movement = false;
    let mut east = grid.clone();

    // first, perform east facing moves
    for i in 0..grid.len() {
        for j in 0..grid[i].len() {
            // check if this is east facing
            if grid[i][j] == '>' {
                // check if next location is empty
                let next = if j == grid[i].len() - 1 { 0 } else { j + 1 };
                if grid[i][next] == '.' {
                    east[i][next] = '>';
                    east[i][j] = '.';
                    movement = true;
                }
            }
        }
    }

    // then, perform south facing moves on a new copy
    let mut south = east.clone();
    for i in 0..grid.len() {
        for j in 0..grid[i].len() {
            // check if this is south facing
            if east[i][j] == 'v' {
                // check if next location is empty
                let next = if i == east.len() - 1 { 0 } else { i + 1 };
                if east[next][j] == '.' {
                    south[next][j] = 'v';
                    south[i][j] = '.';
                    movement = true;
                }
            }
        }
    }
    
    // return the new grid, if anything changed
    if movement {
        return Some(south);
    }
    return None;
}

fn main() {
    // get initial state of grid
    let mut grid = parse_input(FILENAME).unwrap();

    // println!("Got starting grid:");
    // plot(&grid);

    // iterate until we stop getting updates
    let mut i = 0;
    loop {
        i += 1;
        match step(&grid) {
            Some(g) => {
                grid = g;
                // println!("Iteration #{}:", i);
                // plot(&grid);
            },
            None => {
                println!("Found the final grid for Part A at step {}", i);
                break;
            }
        }
    }
}






