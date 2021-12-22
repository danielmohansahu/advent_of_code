// Rebooting Reactors
// https://adventofcode.com/2021/day/22

use std::vec::Vec;
use std::ops::Range;
use anyhow::Result;
use std::fs::File;
use std::io::{BufRead, BufReader};

// static FILENAME: &str = "example.txt";
static FILENAME: &str = "example2.txt";
// static FILENAME: &str = "steps.txt";

struct Step {
    action: bool,
    xrange: Range<i64>,
    yrange: Range<i64>,
    zrange: Range<i64>
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
        let (xmin,xmax): (i64,i64) = (
            xrange_split.next().unwrap().parse().unwrap(),
            xrange_split.next().unwrap().parse().unwrap());
        step.xrange = xmin..(xmax+1);   // convert to inclusive range

        // parse xrange
        let mut yrange_split = ranges.next().unwrap()[2..].split("..");
        let (ymin,ymax): (i64,i64) = (
            yrange_split.next().unwrap().parse().unwrap(),
            yrange_split.next().unwrap().parse().unwrap());
        step.yrange = ymin..(ymax+1);   // convert to inclusive range

        // parse xrange
        let mut zrange_split = ranges.next().unwrap()[2..].split("..");
        let (zmin,zmax): (i64,i64) = (
            zrange_split.next().unwrap().parse().unwrap(),
            zrange_split.next().unwrap().parse().unwrap());
        step.zrange = zmin..(zmax+1);   // convert to inclusive range

        // push step
        steps.push(step);
    }

    Ok(steps)
}

fn process_step(cube: &mut Vec<Vec<Vec<bool>>>, step: &Step, xstep: i64, ystep: i64, zstep: i64) {
    // perform the operations in 'step' on the given Cube

    for x in step.xrange.clone() {
        for y in step.yrange.clone() {
            for z in step.zrange.clone() {
                // perform index conversion
                let (x,y,z) = (x + xstep, y + ystep, z + zstep);

                // skip if out of bounds
                if (0 <= x) && (x < cube.len() as i64)
                    && (0 <= y) && (y < cube[0].len() as i64)
                    && (0 <= z) && (z < cube[0][0].len() as i64) {
                    cube[x as usize][y as usize][z as usize] = step.action;

                }
            }
        }
    }
}

fn get_size(steps: &Vec<Step>) -> ((usize,usize,usize),(i64,i64,i64)) {
    // get the dimensions of the grid we need to construct
    // to process all these steps, as well as the minimum value of each range
    let (mut xrange, mut yrange, mut zrange) = (0..0,0..0,0..0);
    for step in steps {
        if step.xrange.start < xrange.start {
            xrange.start = step.xrange.start;
        }
        if step.xrange.end > xrange.end {
            xrange.end = step.xrange.end;
        }
        if step.yrange.start < yrange.start {
            yrange.start = step.yrange.start;
        }
        if step.yrange.end > yrange.end {
            yrange.end = step.yrange.end;
        }
        if step.zrange.start < zrange.start {
            zrange.start = step.zrange.start;
        }
        if step.zrange.end > zrange.end {
            zrange.end = step.zrange.end;
        }
    }
    let (xsize,ysize,zsize) = (xrange.end - xrange.start, yrange.end - yrange.start, zrange.end - zrange.start);
    return ((xsize as usize, ysize as usize, zsize as usize),(-xrange.start,-yrange.start,-zrange.start));
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
    // get the steps of operation
    let steps = parse_input(FILENAME).unwrap();

    // For Part A we just need to turn on/off some cells.
    //  We're bounded to a -50,50 cuboid
    let mut cubeA = vec![vec![vec![false; 101]; 101]; 101];

    // perform the steps of part A
    for (i,step) in steps.iter().enumerate() {
        println!("Processing step #{}", i+1);
        process_step(&mut cubeA, step, 50, 50, 50);
    }
    println!("Part A: got {} cubes on.", count(&cubeA));

    // for Part B we don't have a given size restriction. We need to calculate it
    let ((xsize,ysize,zsize),(xstep,ystep,zstep)) = get_size(&steps);
    let mut cubeB = vec![vec![vec![false; xsize]; ysize]; zsize];

    // same as Part A, but bigger
    for (i,step) in steps.iter().enumerate() {
        println!("Part B: Processing step #{}", i+1);
        process_step(&mut cubeB, step, xstep, ystep, zstep);
    }
    println!("Part B: got {} cubes on.", count(&cubeB));



}
