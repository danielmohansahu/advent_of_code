// One should walk warily amongst Octopi
// https://adventofcode.com/2021/day/11

use std::vec::Vec;
use std::collections::HashMap;
use std::fs::File;
use anyhow::Result;

fn step(energy: &mut Vec<Vec<u8>>) -> u32 {
    // update the octopi energy levels
    // 1. Add 1 to all levels
    // 2. Process flashes (each octopi can only flash once)
    // 3. Reset flashed octopi's level to 0

    // first add one to all
    for l in energy.iter_mut() {
        for e in l.iter_mut () {
            *e += 1;
        }
    }

    // then process flashes
    let mut flashes = 0;
    loop {
        let mut new_flashes = 0;

        for i in 0..energy.len() {
            for j in 0..energy[i].len() {
                if energy[i][j] > 9 {
                    // we got a flash!
                    new_flashes += 1;
                    energy[i][j] = 0;         // we mark this as ineligible to flash again

                    // go through and increment valid neighbors
                    if (i != 0) && (j != 0) && (energy[i-1][j-1] != 0) {
                        // top left
                        energy[i-1][j-1] += 1;
                    }
                    if (i != 0) && (energy[i-1][j] != 0) {
                        // top
                        energy[i-1][j] += 1;
                    }
                    if (i != 0) && (j != energy[i].len() - 1) && (energy[i-1][j+1] != 0) {
                        // top right
                        energy[i-1][j+1] += 1;
                    }
                    if (j != 0) && (energy[i][j-1] != 0) {
                        // left
                        energy[i][j-1] += 1;
                    }
                    if (j != energy[i].len() - 1) && (energy[i][j+1] != 0) {
                        // right
                        energy[i][j+1] += 1;
                    }
                    if (i != energy.len() - 1) && (j != 0) && (energy[i+1][j-1] != 0) {
                        // bottom left
                        energy[i+1][j-1] += 1;
                    }
                    if (i != energy.len() - 1) && (energy[i+1][j] != 0) {
                        // bottom
                        energy[i+1][j] += 1;
                    }
                    if (i != energy.len() - 1) && (j != energy[i].len() - 1) && (energy[i+1][j+1] != 0) {
                        // bottom right
                        energy[i+1][j+1] += 1;
                    }
                }
            }
        }

        // check if nobody flashed this round, which is our stop condition
        if new_flashes == 0 {
            break;
        }
        flashes += new_flashes;
    }

    return flashes;
}

fn main() {
    // parse input into vectors
    let mut current: Vec<Vec<u8>> = Vec::new();
    for line in include_str!("../octopi.txt").lines() {
        let mut row: Vec<u8> = Vec::new();
        for char_ in line.chars() {
            row.push(char_.to_digit(10).unwrap() as u8);
        }
        current.push(row);
    }

    // println!("Got initial energies: {:?}", current);
    let mut flash_count = 0;
    for i in 0..100 {
        flash_count += step(&mut current);
        println!("Processing step {}: \n{:?}", i + 1, current);
    }
    println!("Part A: Got a flash count of {}", flash_count);

}
