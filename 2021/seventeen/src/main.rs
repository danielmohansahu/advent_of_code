// Like shooting probes in a trench.
// https://adventofcode.com/2021/day/17

use std::cmp;

// puzzle inputs
// static XMIN: i64 = 20;
// static XMAX: i64 = 30;
// static YMIN: i64 = -10;
// static YMAX: i64 = -5;
static XMIN: i64 = 70;
static XMAX: i64 = 96;
static YMIN: i64 = -179;
static YMAX: i64 = -124;

// convenience enum
enum ProbeLoc {
    UNDERSHOOT, // still low in X or high in Y
    OVERSHOOT,  // too low in Y or high in X
    ONTARGET   // within target bounds
}

fn check(x: &i64, y: &i64) -> ProbeLoc {
    // check if the given x/y locations are on target
    if (x > &XMAX) || (y < &YMIN) {
        return ProbeLoc::OVERSHOOT;
    } else if (x >= &XMIN && x <= &XMAX) && (y >= &YMIN && y <= &YMAX) {
        return ProbeLoc::ONTARGET;
    } else {
        // I believe this is the only possibility
        return ProbeLoc::UNDERSHOOT;
    }
}

fn step(x: &mut i64, y: &mut i64, dx: &mut i64, dy: &mut i64) {
    // update position
    *x += *dx;
    *y += *dy;
    
    // update velocities
    *dy -= 1;
    if *dx < 0 {
        *dx += 1;
    } else if *dx > 0 {
        *dx -= 1;
    }
}

fn fire(dx: &i64, dy: &i64) -> i64 {
    // evaluate the given initial starting velocities
    //  returns max height achieved if successful, i64::MIN otherwise
    let (mut x, mut y, mut dx, mut dy) = (0, 0, *dx, *dy);
    let mut ymax = i64::MIN;

    // loop until stop conditions are met
    loop {
        // step forward in velocities
        step(&mut x, &mut y, &mut dx, &mut dy);
        
        match check(&x, &y) {
            ProbeLoc::OVERSHOOT => break,   // stop condition
            ProbeLoc::UNDERSHOOT => {
                // update y value
                ymax = cmp::max(y, ymax);
                continue;
            }
            ProbeLoc::ONTARGET => {
                // return maximum y achieved
                return cmp::max(y, ymax);
            }
        }
    }
    // indicate we didn't succeed
    return i64::MIN;
}

fn main() {
    // initialize loop variables for Part A
    let mut max_height: i64 = i64::MIN;
    let mut max_height_xy: (i64,i64) = (0,0);
    
    // initialize variables for Part B
    let mut successes = 0;

    // for part A, probe all possible starting velocities
    for dx in 1..XMAX+1 {
        // @TODO this is arbitrary - how to upper bound Y?
        for dy in YMIN..(-YMIN + 1) {
            // test this trajectory
            let local_maxima = fire(&dx, &dy);
            // println!("\t ({},{}) -> {}", dx, dy, local_maxima);
            if local_maxima > max_height {
                max_height = local_maxima;
                max_height_xy = (dx, dy);
                println!("Found new max height {} from ({},{})", max_height, dx, dy);
            }
            // also track success count for part B
            if local_maxima != i64::MIN {
                successes += 1;
            }
        }
    }

    println!("Part A: Found max height {} for starting velocities {:?}", max_height, max_height_xy);
    println!("Part B: Found {} successful velocities.", successes);
}






