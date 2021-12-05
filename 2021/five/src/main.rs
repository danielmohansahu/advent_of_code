// determine where geothermal vents collide
// https://adventofcode.com/2021/day/5

use std::vec::Vec;
use std::fs::File;
use std::io::{BufRead, BufReader};

// const FILENAME : &str = "example.txt";
const FILENAME : &str = "lines.txt";

// convenience point definition
struct Point {
    x: i32,
    y: i32
}

fn parse_input(filename: &str) -> Vec<(Point,Point)> {
    // parse the given input file and extract all line segments
    
    // initialize result
    let mut result: Vec<(Point,Point)> = Vec::new();

    // set up a reader and file object
    let file = File::open(filename).expect("Unable to open file.");
    let reader = BufReader::new(file);

    for line in reader.lines() {
        // read this line and replace some characters for easier parsing
        let mut line = line.expect("Unable to parse line.");
        line = line.replace(" -> ", ",");

        // parse this line into component points
        let mut split = line.split(',');
        let x1: i32 = split.next().expect("Missing element").parse().expect("Found non-digit");
        let y1: i32 = split.next().expect("Missing element").parse().expect("Found non-digit");
        let x2: i32 = split.next().expect("Missing element").parse().expect("Found non-digit");
        let y2: i32 = split.next().expect("Missing element").parse().expect("Found non-digit");
        
        // convert to points and push to result
        result.push((Point{x : x1, y : y1}, Point{x : x2, y : y2}));
    }

    return result;
}

fn extents(segments: &Vec<(Point,Point)>) -> Point {
    // returns the largest point we care about
    let mut pt: Point = Point{x: 0, y: 0};

    for segment in segments.iter() {
        let (l1,l2) = (&segment.0, &segment.1);
        if l1.x > pt.x {
            pt.x = l1.x;
        }
        if l2.x > pt.x {
            pt.x = l2.x;
        }
        if l1.y > pt.y {
            pt.y = l1.y;
        }
        if l2.y > pt.y {
            pt.y = l2.y;
        }
    }
    return pt;
}

fn draw(map: &mut Vec<Vec<u32>>, segment: &(Point,Point)) {
    // mark all cells in the given map that this segment touches
    let (start, end) = (&segment.0, &segment.1);

    // check if this is a horizontal or vertical line, and
    //  whether we need to walk forward or backwards
    let vert: bool = start.x == end.x;
    let reverse: bool = if vert {end.y < start.y} else {end.x < start.x};
    println!("Vert: {}, Reverse: {}", vert, reverse);
    
    // create convenience function to increment
    let increment = |point: &mut Point| {
        if vert && reverse {
            // walk backwards in y
            point.y -= 1;
        } else if vert && !reverse {
            // walk forwards in y
            point.y += 1;
        } else if !vert && reverse {
            // walk backwards in x
            point.x -= 1;
        } else if !vert && !reverse {
            // walk forwards in x
            point.x += 1;
        }
    };

    // create a 'current point' for drawing
    let mut pt: Point = Point{x: start.x, y: start.y};
    map[pt.x as usize][pt.y as usize] += 1;

    // iterate through the line semgent, drawing as we go
    while !(pt.x == end.x && pt.y == end.y) {
        // increment to next point
        increment(&mut pt);
        // draw this point
        println!("Drawing point: ({},{})", pt.x,pt.y);
        map[pt.x as usize][pt.y as usize] += 1;
    }
}

fn main() {
    // parse input file
    let vents = parse_input(FILENAME);

    // determine the maximum extent we care about, and construct a map of appropriate size
    let upper_bound = extents(&vents);
    let (max_x,max_y): (usize,usize) = ((upper_bound.x + 1) as usize, (upper_bound.y + 1) as usize);
    let mut map: Vec<Vec<u32>> = vec![vec![0; max_y]; max_x];

    // iterate through all segments, drawing them on the map
    for segment in vents.iter() {
        // skip diagonal lines
        if (segment.0.x != segment.1.x) && (segment.0.y != segment.1.y) {
            continue;
        }
        draw(&mut map, &segment);
    }

    // count the number of points with more than one intersection
    let mut count = 0;
    for i in 0..max_x {
        for j in 0..max_y {
            if map[i][j] > 1 {
                count += 1;
                println!("Found {} intersections at ({},{})", map[i][j], i, j);
            }
        }
    }

    println!("Found {} intersections total.", count);


}
