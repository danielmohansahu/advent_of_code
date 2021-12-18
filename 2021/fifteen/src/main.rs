// Ahh, Djikstra.
// https://adventofcode.com/2021/day/15

// input data
// static FILENAME: &str = "example.txt";
static FILENAME: &str = "chitons.txt";

use std::vec::Vec;
use anyhow::Result;
use std::fs::File;
use std::collections::HashMap;
use std::io::{BufRead, BufReader};

struct Node {
    risk: u32,      // risk of this node
    dist: u32,      // risk from start to this node (inclusive)
}

// construct a grid of nodes from the given file
fn parse_input(filename: &str) -> Result<Vec<Vec<Node>>> {
    // initialize output grid
    let mut grid: Vec<Vec<Node>> = Vec::new();

    // set up a reader and file object
    let file = File::open(filename)?;
    let reader = BufReader::new(file);

    // parse all the lines into our various arrays
    for line in reader.lines() {
        let line = line?;

        // initialize new row
        let mut row: Vec<Node> = Vec::new();

        // parse each element into a digit
        for char_ in line.chars() {
            let char_: u32 = char_.to_digit(10).unwrap();
            row.push(Node{risk: char_, dist: u32::MAX});
        }
        grid.push(row);
    }

    Ok(grid)
}

// get vector of tuple (neighbors)
fn get_neighbors(current: &(usize,usize), dim: &(usize,usize)) -> Vec<(usize,usize)> {
    // initialize variables
    let mut res = Vec::new();
    let (i,j) = *current;
    let (w,l) = *dim;

    // check bounds
    if i != 0 {
        res.push((i-1,j));
    } 
    if i != (w - 1) {
        res.push((i+1,j));
    }
    if j != 0 {
        res.push((i,j-1));
    }
    if j != (l - 1) {
        res.push((i,j+1));
    }

    return res;
}

fn djikstra(grid: &mut Vec<Vec<Node>>) -> u32 {
    // initialize unvisited nodes
    let mut unvisited: HashMap<(usize,usize),u32> = HashMap::new();
    for i in 0..grid.len() {
        for j in 0..grid[0].len() {
            unvisited.insert((i,j),u32::MAX);
        }
    }

    // get size of grid for convenience
    let dim = (grid.len(), grid[0].len());

    // initialize start node (0,0)
    let mut current = (0,0);
    let (i,j) = current;
    grid[i][j].dist = 0;

    // begin main execution loop
    while unvisited.len() != 0 {
        let (i,j) = current;
        // println!("Checking {:?} - cost {}", current, grid[i][j].dist);

        // update neighbors of current node
        for neighbor in get_neighbors(&current, &dim) {
            if unvisited.contains_key(&neighbor) {
                let (x,y) = neighbor;
                // check what new cost would be
                let dist = grid[i][j].dist + grid[x][y].risk;
                // update costs if lower than extant
                if dist < grid[x][y].dist {
                    grid[x][y].dist = dist;
                    assert!(unvisited.contains_key(&neighbor));
                    *unvisited.entry(neighbor).or_insert(u32::MAX) = dist;
                }
            }
        }

        // remove this node from the visited set
        unvisited.remove(&current);

        // find next lowest cost node
        let mut score = u32::MAX;
        for (k,v) in &unvisited {
            if v < &score {
                score = *v;
                current = *k;
            }
        }
    }

    // return distance of final node
    grid[grid.len()-1][grid[0].len()-1].dist
}

fn print_costs(grid: &Vec<Vec<Node>>) {
    for i in 0..grid.len() {
        let mut string = String::new();
        for j in 0..grid[i].len() {
            let mut repr = grid[i][j].dist.to_string();
            for k in 0..(4-repr.chars().count()) {
                repr.push(' ');
            }
            string.push_str(&repr);
        }
        println!("{:?}", string);
    }
}

fn main() {
    // load grid
    let mut grid = parse_input(FILENAME).unwrap();

    // run djikstra's algorithm
    let risk = djikstra(&mut grid);
    print_costs(&grid);
    println!("Part A: Got path with risk {}", risk);

}