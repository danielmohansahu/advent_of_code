// Origami?
// https://adventofcode.com/2021/day/13

use std::vec::Vec;
use std::fs::File;
use std::io::{BufRead, BufReader};
use anyhow::Result;

// const FILENAME: &str = "example.txt";
const FILENAME: &str = "input.txt";

// parse the input file to get the starting paper and fold instructions
fn parse_input(filename: &str) -> Result<(Vec<(u32,u32)>,Vec<(char,u32)>)> {

    // initialize outputs
    let mut marking: Vec<(u32,u32)> = Vec::new();
    let mut folds: Vec<(char,u32)> = Vec::new();

    // set up a reader and file object
    let file = File::open(filename)?;
    let reader = BufReader::new(file);

    // parse all the lines into our various arrays
    let mut at_folds = false;
    for line in reader.lines() {
        let line = line?;

        // check if this is an empty line marking the folds section
        if line == "" {
            at_folds = true;
            continue;
        }

        // check if we are at the folds section
        if !at_folds {
            // add these indices to our output
            let mut split = line.split(',');
            marking.push((
                split.next().unwrap().parse()?,
                split.next().unwrap().parse()?));
        } else {
            // consider only last part of the string
            let substring = line.split_whitespace().last().unwrap();
            let mut split = substring.split('=');
            let direction = split.next().unwrap().chars().nth(0).unwrap();
            let index = split.next().unwrap().parse().unwrap();
            // println!("Processed substring: {:?} into ({},{})", substring, direction, index);
            folds.push((direction, index));
        }
    }

    Ok((marking,folds))
}

fn construct_grid(markings: &Vec<(u32,u32)>) -> Vec<Vec<bool>> {
    // create a grid containing the given markings
    
    // first get the total size
    let (mut max_x, mut max_y): (u32, u32) = (0, 0);
    for (x,y) in markings {
        if x > &max_x {
            max_x = *x;
        }
        if y > &max_y {
            max_y = *y;
        }
    }

    // construct result grid
    let mut result: Vec<Vec<bool>> = vec![vec![false; (max_y + 1) as usize]; (max_x + 1) as usize];

    // mark markings
    for (x,y) in markings {
        result[*x as usize][*y as usize] = true;
    }
    return result;
}

fn fold_horizontal(grid: &Vec<Vec<bool>>, index: u32) -> Vec<Vec<bool>> {
    // fold horizontally along the given index

    // check that this isn't an edge case
    assert_eq!(grid.len() % 2, 1);
    
    let new_x = (grid.len() - 1) / 2;
    let new_y = grid[0].len();
    
    // initialize result grid
    let mut result = vec![vec![false; new_y]; new_x];

    // combine existing and newly folded data into grid
    for i in 0..new_x {
        for j in 0..new_y {
            // copy old data
            result[i][j] = grid[i][j];
            // modify to include folded data
            if grid[grid.len() - i - 1][j] {
                result[i][j] = true;
            }
        }
    }
    return result;
}

fn fold_vertical(grid: &Vec<Vec<bool>>, index: u32) -> Vec<Vec<bool>> {
    // fold vertically along the given index

    // check that this isn't an edge case
    assert_eq!(grid[0].len() % 2, 1);
    let new_x = grid.len();
    let new_y = (grid[0].len() - 1) / 2;
    
    // initialize result grid
    let mut result = vec![vec![false; new_y]; new_x];

    // combine existing and newly folded data into grid
    for i in 0..new_x {
        for j in 0..new_y {
            // copy old data
            result[i][j] = grid[i][j];
            // modify to include folded data
            if grid[i][grid[i].len() - j - 1] {
                result[i][j] = true;
            }
        }
    }
    return result;
}

fn count(grid: &Vec<Vec<bool>>) -> u32 {
    // count the number of marks
    let mut count = 0;
    for i in 0..grid.len() {
        for j in 0..grid[i].len() {
            if grid[i][j] {
                count += 1;
            }
        }
    }
    count
}

fn print_grid(grid: &Vec<Vec<bool>>) {
    // prett(ier) printing of grid, with '#' marking nonzero elements and '.' for 0.
    for j in 0..grid[0].len() {
        // row as a string
        let mut row: String = String::new();
        for i in 0..grid.len() {
            if grid[i][j] {
                row.push('#');
            } else {
                row.push('.');
            }
        }
        println!("{:?}", row);
    }
    
}

fn main() {
    // parse input
    let (markings, folds) = parse_input(FILENAME).unwrap();

    // construct a starting grid from the inputs
    let initial_grid = construct_grid(&markings);

    // perform desired fold instructions
    let mut current = initial_grid.clone();
    for (direction, index) in folds {
        println!("Total count before folding: {}", count(&current));
        println!("Folding at {} in {}", index, direction);
        current = match direction {
            'x' => fold_horizontal(&current, index),
            'y' => fold_vertical(&current, index),
            _   => panic!("Unexpected fold direction!")
        };
    }
    println!("Final count: {}", count(&current));
    print_grid(&current);
}




















