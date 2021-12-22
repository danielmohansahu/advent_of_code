// Rebooting Reactors
// https://adventofcode.com/2021/day/22

use std::vec::Vec;
use std::ops::Range;
use anyhow::Result;
use std::fs::File;
use std::io::{BufRead, BufReader};

// static FILENAME: &str = "example.txt";
static FILENAME: &str = "steps.txt";

struct Step {
    action: bool,
    xrange: Range<i32>,
    yrange: Range<i32>,
    zrange: Range<i32>
}

fn parse_input(filename: &str) -> Result<Vec<Step>> {
    // Parse the given file, returning the sequence of steps to perform
    let mut steps = Vec::new();

    // set up a reader and file object
    let file = File::open(filename)?;
    let reader = BufReader::new(file);

    // parse inputs
    for line in reader.lines() {
        let line = line?;
        // initialize result
        let mut step = Step {action: false, xrange: 0..0, yrange: 0..0, zrange: 0..0};

        // parse line
        let mut split_whitespace = line.split_whitespace();
        step.action = match split_whitespace.next().unwrap() {
            "on" => true,
            "off" => false,
            _ => panic!("Got unexpected string."),
        };

        // parse remaining whitespace to get ranges
        let mut ranges = split_whitespace.next().unwrap().split(',');

        // parse xrange
        let mut xrange_split = ranges.next().unwrap()[2..].split("..");
        let (xmin,xmax): (i32,i32) = (
            xrange_split.next().unwrap().parse().unwrap(),
            xrange_split.next().unwrap().parse().unwrap());
        step.xrange = xmin..(xmax+1);   // convert to inclusive range

        // parse xrange
        let mut yrange_split = ranges.next().unwrap()[2..].split("..");
        let (ymin,ymax): (i32,i32) = (
            yrange_split.next().unwrap().parse().unwrap(),
            yrange_split.next().unwrap().parse().unwrap());
        step.yrange = ymin..(ymax+1);   // convert to inclusive range

        // parse xrange
        let mut zrange_split = ranges.next().unwrap()[2..].split("..");
        let (zmin,zmax): (i32,i32) = (
            zrange_split.next().unwrap().parse().unwrap(),
            zrange_split.next().unwrap().parse().unwrap());
        step.zrange = zmin..(zmax+1);   // convert to inclusive range

        // push step
        steps.push(step);
    }

    Ok(steps)
}

fn process_step(cube: &mut Vec<Vec<Vec<bool>>>, step: &Step) {
    // perform the operations in 'step' on the given Cube

    for x in step.xrange.clone() {
        for y in step.yrange.clone() {
            for z in step.zrange.clone() {
                // perform index conversion
                let (x,y,z) = (x + 50, y + 50, z + 50);

                // skip if out of bounds
                if (0 <= x) && (x <= 101) && (0 <= y) && (y <= 101) && (0 <= z) && (z <= 101) {
                    cube[x as usize][y as usize][z as usize] = step.action;

                }
            }
        }
    }
}

fn count(cube: &Vec<Vec<Vec<bool>>>) -> u64 {
    let mut count = 0;
    for x in 0..cube.len() {
        for y in 0..cube[x].len() {
            for z in 0..cube[y].len() {
                if cube[x][y][z] {
                    count += 1;
                }
            }
        }
    }
    return count;
}

fn main() {
    // For Part A we just need to turn on/off some cells.
    //  We're bounded to a -50,50 cuboid
    let mut cube = vec![vec![vec![false; 101]; 101]; 101];

    // get the steps of operation
    let steps = parse_input(FILENAME).unwrap();

    // perform the steps of part A
    for step in &steps {
        process_step(&mut cube, step);
    }
    println!("Part A: got {} cubes on.", count(&cube));
}
