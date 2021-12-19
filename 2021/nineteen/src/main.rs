// ICP, for Beacons and Scanners
// https://adventofcode.com/2021/day/19

// Approach for Part A:
//  For each scanner Pair
//  1) Rotate second scanner by ever permutation of XYZ rotation
//  2) Offset permuted points by each point in scanner #1
//  3) If 12 matches, that rotation / translation represents the 
//      coordinate transform from 2->1

static FILENAME: &str = "example.txt";
// static FILENAME: &str = "readings.txt";

use std::fmt;
use std::vec::Vec;
use anyhow::Result;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Copy, Clone, Debug)]
struct Point {
    x: i32,
    y: i32,
    z: i32
}

// display for my point, for convenience
impl fmt::Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {}, {})", self.x, self.y, self.z)
    }
}

fn parse_input(filename: &str) -> Result<Vec<Vec<Point>>> {
    // extract input into a vector of scanner points
    let mut scanners: Vec<Vec<Point>> = Vec::new();

    // set up a reader and file object
    let file = File::open(filename)?;
    let reader = BufReader::new(file);

    // parse all the lines into our various arrays
    // initialize single scanner
    let mut scanner: Vec<Point> = Vec::new();
    for line in reader.lines() {
        let line = line?;

        // check if we should start a new scanner
        if line == "" {
            scanners.push(scanner);
            scanner = Vec::new();
            continue;
        } else if &line[..3] == "---" {
            // skip these headers
            continue;
        }

        // convert line to point
        let mut split = line.split(',');
        scanner.push(Point{ x: split.next().unwrap().parse().unwrap(),
                            y: split.next().unwrap().parse().unwrap(),
                            z: split.next().unwrap().parse().unwrap()});
    }
    // make sure to push last scanner
    scanners.push(scanner);

    // return result
    Ok(scanners)
}

fn rotate(points: &Vec<Point>, rpy: &Point) -> Vec<Point> {
    // rotate the given set of points by the given RPY
    let mut new = Vec::new();
    let (r,p,y) = (rpy.x, rpy.y, rpy.z);
    let (rf,pf,yf) = (r as f32, p as f32, y as f32);
    let (rsin, rcos) = (rf.to_radians().sin() as i32, rf.to_radians().cos() as i32);
    let (psin, pcos) = (pf.to_radians().sin() as i32, pf.to_radians().cos() as i32);
    let (ysin, ycos) = (yf.to_radians().sin() as i32, yf.to_radians().cos() as i32);

    for p in points.iter() {
        new.push(Point{
            x: p.x * ycos * pcos + p.y * (ycos * psin * rsin - ysin * rcos) + p.z * (ycos * psin * rcos + ysin * rsin),
            y: p.x * ysin * pcos + p.y * (ysin * psin * rsin + ycos * rcos) + p.z * (ysin * psin * rcos - ycos * rsin),
            z: p.x * -psin + p.y * pcos * rsin + p.z * pcos * rcos});
    }
    return new;
}

fn translate(points: &Vec<Point>, xyz: &Point) -> Vec<Point> {
    // translate the given set of points by the given XYZ
    let mut new = Vec::new();
    for p in points.iter() {
        new.push(Point{
            x: p.x + xyz.x,
            y: p.y + xyz.y,
            z: p.z + xyz.z});
    }
    return new;
}

fn icp(src: &Vec<Point>, tgt: &Vec<Point>) -> Option<(Point,Point)> {
    // perform ICP (not really?) between src and tgt, optionally
    // returning a tuple representing the transformation (RPY,XYZ)
    // if at least 12 matches were found

    for r in 0..4 {
        for p in 0..4 {
            for y in 0..4 {
                // get rotation (in degrees, for no good reason)
                let rpy = Point{x: 90 * r, y: 90 * p, z: 90 * y};
                let candidate = rotate(tgt, &rpy);

                // iterate through points in src, attempting to match with transformation
                for point in src.iter() {
                    let transform = Point{
                        x: point.x - candidate[0].x,
                        y: point.y - candidate[0].y,
                        z: point.z - candidate[0].z};
                    // get a local copy of candidate, transformed to match the first point of src
                    let candidate = translate(&candidate, &transform);

                    // check for success - we need at least 12 matches
                    let mut matches = 0;
                    for i in 0..src.len() {
                        for pt in &candidate {
                            if (pt.x == src[i].x) && (pt.y == src[i].y) && (pt.z == src[i].z) {
                                matches += 1;
                                if matches == 12 {
                                    println!("Found 12 good matches!");
                                    return Some((rpy,transform));
                                }
                            }
                        }
                    }
                    // sanity check we got at least 1 (the one we translated to!)
                    assert!(matches > 0);
                }
            }
        }
    }
    // if we get this far, we failed. sad
    None
}

fn main() {
    // parse input
    let scanners = parse_input(FILENAME).unwrap();

    // get overlap between each pair of scanners, storing 
    // transforms in a map
    let mut transforms: HashMap<(usize,usize),(Point,Point)> = HashMap::new();
    for i in 0..scanners.len() {
        for j in (i+1)..scanners.len() {
            println!("Comparing {} to {}", i, j);
            let transform = icp(&scanners[i], &scanners[j]);
            if transform.is_some() {
                let transform = transform.unwrap();
                println!("Found transform from {}->{}: ({},{})", i, j, transform.0, transform.1);
                transforms.insert((i,j),transform);
            }
        }
    }
}
