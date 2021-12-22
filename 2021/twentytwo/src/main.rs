// Rebooting Reactors
// https://adventofcode.com/2021/day/22

use std::cmp;
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

    // narrow range to only contain the subset which overlaps
    let xrange = cmp::max(step.xrange.start, -xstep)..cmp::min(step.xrange.end, cube.len() as i64 + xstep);
    let yrange = cmp::max(step.yrange.start, -ystep)..cmp::min(step.yrange.end, cube[0].len() as i64 + ystep);
    let zrange = cmp::max(step.zrange.start, -zstep)..cmp::min(step.zrange.end, cube[0][0].len() as i64 + zstep);

    // println!("Ranges: {:?}, {:?}, {:?}", xrange, yrange, zrange);

    for x in xrange {
        for y in yrange.clone() {
            for z in zrange.clone() {
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

fn size(range: &Range<i64>) -> u64 {
    // return the size of the given range
    if range.start >= range.end {
        return 0;
    } else {
        return (range.end - range.start) as u64; 
    }
}

fn no_overlap(step: &Step, steps: &[Step]) -> u64 {
    // return the number of cells that step has that _don't_
    // overlap with the other given steps
    println!("Comparing step to {} following", steps.len());

    // start our count with the initial number of points
    let count = size(&step.xrange) * size(&step.yrange) * size(&step.zrange);
    let mut overlap = 0;

    // construct a list of steps to look at that overlap with this step
    let mut overlapping_steps = Vec::new();
    for s in steps {
        // construct a new step that represents the overlap in range
        let xrange = cmp::max(step.xrange.start, s.xrange.start)..cmp::min(step.xrange.end, s.xrange.end);
        let yrange = cmp::max(step.yrange.start, s.yrange.start)..cmp::min(step.yrange.end, s.yrange.end);
        let zrange = cmp::max(step.zrange.start, s.zrange.start)..cmp::min(step.zrange.end, s.zrange.end);

        // only add if this contains nonzero ranges
        let o = size(&xrange) * size(&yrange) * size(&zrange);
        if o != 0 {
            overlap += o;
            overlapping_steps.push(Step { action: false, xrange: xrange, yrange: yrange, zrange: zrange });
        }
    }

    // for each of these overlapping steps, get their total size minus any overlap they have with all other steps
    for (i,s1) in overlapping_steps.iter().enumerate() {
        // construct a copy of the remaining steps with ith index removed
        let mut subset = Vec::new();
        for (j,s) in overlapping_steps.iter().enumerate() {
            if i != j {
                subset.push(Step { action: false, xrange: s.xrange.clone(), yrange: s.yrange.clone(), zrange: s.zrange.clone() });
            }
        }
        let o = no_overlap(&s1, &subset);
        // decrement
        assert!(o <= overlap);
        overlap -= o;
    }
    
    // the result (i.e. the amount of the given step that _doesn't_ overlap with anything)
    assert!(count >= overlap);
    return count - overlap;
}

fn main() {
    // get the steps of operation
    let steps = parse_input(FILENAME).unwrap();

    // For Part A we just need to turn on/off some cells.
    //  We're bounded to a -50,50 cuboid
    let mut cubeA = vec![vec![vec![false; 101]; 101]; 101];

    // perform the steps of part A
    for (i,step) in steps.iter().enumerate() {
        // println!("Processing step #{}", i+1);
        process_step(&mut cubeA, step, 50, 50, 50);
    }
    println!("Part A: got {} cubes on.", count(&cubeA));

    // for Part B we work backwards to get the total of cubes on count
    let mut on_count = 0;
    for (i,step) in steps.iter().rev().enumerate() {
        // println!("Part B: Processing step #{}", steps.len() - i);

        // if this step turns things on, find the cells without overlap with the rest of the ranges
        if step.action {
            let tmp = no_overlap(step, &steps[(steps.len() - i)..]);
            on_count += tmp;
            println!("Step #{} found {} on: {:?}, {:?}, {:?}", steps.len() - i, tmp, step.xrange, step.yrange, step.zrange);
        }

    }
    println!("Part B: got {} cubes on.", on_count);



}
