// Analyzing heightmaps to avoid asphyxiation
// https://adventofcode.com/2021/day/9

use std::vec::Vec;
use std::fs::File;
use std::io::{BufRead, BufReader};

// const FILENAME: &str = "example.txt";
const FILENAME: &str = "heightmap.txt";

// convenience type for defining low point attributes
struct LowPoint {
    x: usize,   // the row index
    y: usize,   // the column index
    e: u8,      // the element value
    basin: u64  // the associated basin size
}

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
fn local_minima(heightmap: & Vec<Vec<u8>>) -> Vec<LowPoint> {
    // initialize result
    let mut minima: Vec<LowPoint> = Vec::new();
    
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
            // println!("Local minima detected at ({},{}): {}", i, j, e);
            let basin_size = basin_size(&heightmap, i, j);
            minima.push(LowPoint {x: i, y: j, e: e, basin: basin_size});
        }
    }

    return minima;
}

// find the size of the basin associated with the given low point
fn basin_size(heightmap: & Vec<Vec<u8>>, x: usize, y: usize) -> u64 {
    // search surrounding points until we encounter a 9 (stop condition)
    
    let mut members: Vec<(usize,usize)> = Vec::new();
    members.push((x,y));

    // iteratively add unique neighbors
    let mut last_size = 0;
    loop {
        // check stop condition - we haven't made any progress
        if last_size == members.len() {
            break;
        }
        last_size = members.len();

        // collect a list of new members
        let mut new_members: Vec<(usize,usize)> = Vec::new();
        for m in members.iter() {
            // find all associated neighbors within bounds
            let mut potentials: Vec<(usize,usize)> = Vec::new();
            if m.0 != 0 {
                potentials.push((m.0 - 1, m.1));
            }
            if m.0 != heightmap.len() - 1 {
                potentials.push((m.0 + 1, m.1));
            }
            if m.1 != 0 {
                potentials.push((m.0, m.1 - 1));
            }
            if m.1 != heightmap[m.0].len() - 1 {
                potentials.push((m.0, m.1 + 1));
            }
            println!("Potential neighbors of ({},{}): {:?}", m.0, m.1, potentials);

            // filter out ones already present in members list
            potentials.retain(|&e| !members.contains(&e));
            potentials.retain(|&e| !new_members.contains(&e));

            // filter out those with values of 9 - walls
            potentials.retain(|&e| heightmap[e.0][e.1] != 9);

            // add to new_members list
            for e in potentials.iter() {
                new_members.push(*e);
            }
        }

        for member in new_members.iter() {
            members.push(*member);
        }
    }

    return members.len() as u64;
}

fn main() {
    // parse input text file into heightmap
    let heightmap = parse_input(FILENAME);

    // for part A we want to detect the values of all local minima
    let points = local_minima(&heightmap);

    let mut risk_level: u64 = 0;
    for point in points.iter() {
        risk_level += point.e as u64 + 1;
    }
    println!("Part A: Risk Level is {}", risk_level);

    // for part B we need to find the basins associated with all local minima
    //  we actually want the three largest ones
    let mut basin_sizes: Vec<u64> = Vec::new();
    for point in points.iter() {
        basin_sizes.push(point.basin);
    }
    basin_sizes.sort();
    
    println!("Got basin sizes: {:?}", basin_sizes);
    println!("Part B: Product of 3 largest basins: {}", basin_sizes[basin_sizes.len() - 1] * basin_sizes[basin_sizes.len() - 2] * basin_sizes[basin_sizes.len() - 3]);


    



}
