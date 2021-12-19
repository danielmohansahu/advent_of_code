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

#[derive(Copy, Clone, Debug, Hash)]
struct Point {
    x: i32,
    y: i32,
    z: i32
}

// display for my point, for convenience
impl fmt::Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({},{},{})", self.x, self.y, self.z)
    }
}

// equality comparison for point
impl PartialEq for Point {
    fn eq(&self, other: &Self) -> bool {
        self.x == other.x && self.y == other.y && self.z == other.z
    }
}
impl Eq for Point {}

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

fn get_match_count(src: &Vec<Point>, tgt: &Vec<Point>) -> u32 {
    // return the number of points that match
    let mut matches = 0;
    for i in 0..src.len() {
        for j in 0..tgt.len() {
            if tgt[j] == src[i] {
                matches += 1;
            }
        }
    }
    return matches;
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
                    for c in candidate.iter() {
                        // get the translation between these two points
                        let transform = Point{
                            x: point.x - c.x,
                            y: point.y - c.y,
                            z: point.z - c.z};

                        // get a local copy of candidate, transformed to match the first point of src
                        let potential = translate(&candidate, &transform);
                        assert_eq!(translate(&rotate(tgt, &rpy),&transform), potential);

                        // check for success
                        let matches = get_match_count(src, &potential);

                        // sanity check we got at least 1 (the one we translated to!)
                        assert!(matches > 0);

                        if matches >= 12 {
                            // println!("Found 12 matches.");
                            return Some((rpy,transform));
                        }
                    }
                }
            }
        }
    }
    // if we get this far, we failed. sad
    None
}

fn transform_to_zero(transforms: &HashMap<usize,Vec<(Point,Point)>>, points: &Vec<Point>, src: usize) -> Vec<Point> {
    // Transform the given cloud from src frame into 0 frame

    assert!(transforms.contains_key(&src));
    let mut result = points.clone();
    for tf in &transforms[&src] {
        let (rpy, xyz) = tf;
        result = translate(&rotate(&result, &rpy), &xyz);
    }
    return result;
}

fn flatten_transforms(size: usize, transforms: &HashMap<(usize,usize),(Point,Point)>) -> HashMap<usize,Vec<(Point,Point)>> {
    // convert the given map of pairs -> transforms and flatten into one
    // that returns the world coordinate transform for each element

    let mut result: HashMap<usize,Vec<(Point,Point)>> = HashMap::new();

    // iteratively build out the map
    loop {
        // check stop condition
        if (0..size).all(|x| result.contains_key(&x)) {
            break;
        }
        // iterate through all frames
        for i in 0..size {
            // skip if we already have this one
            if result.contains_key(&i) {
                continue;
            }

            // otherwise try to add this frame to our list
            let key = (0,i);
            if i == 0 {
                // null operation
                result.insert(i, vec![(Point{x:0,y:0,z:0},Point{x:0,y:0,z:0})]);
            } else if transforms.contains_key(&key) {
                // use this key directly
                result.insert(i, vec![transforms[&key]]);
            } else {
                // we need to chain transforms :/
                // check all transforms that mention us
                for (key,val) in transforms.iter() {
                    let (f1,f2) = key;
                    // check if this requires just one more hop
                    if *f2 == i && result.contains_key(f1) {
                        let mut chain = vec![*val];
                        chain.append(&mut result[f1].clone());
                        result.insert(i, chain);
                    } else {
                        // this doesn't concern us.
                        continue;
                    }
                }
                // we couldn't resolve our transform. continue to the next loop
            }
        }
    }
    return result;
}

fn main() {
    // parse input
    let scanners = parse_input(FILENAME).unwrap();

    // get overlap between each pair of scanners, storing 
    // transforms in a map
    let mut transforms: HashMap<(usize,usize),(Point,Point)> = HashMap::new();
    for i in 0..scanners.len() {
        for j in 0..scanners.len() {
            // skip 1 to 1 mapping
            if i == j {
                continue;
            }
            // println!("  comparing {} to {}", i, j);
            let transform = icp(&scanners[i], &scanners[j]);
            if transform.is_some() {
                let transform = transform.unwrap();
                println!("  found transform from {}->{}: ({},{})", i, j, transform.0, transform.1);
                transforms.insert((i,j),transform);
            }
        }
    }

    // construct a new transform set from 0<-ALL
    let transforms_to_zero = flatten_transforms(scanners.len(), &transforms);

    println!("Found the following transforms:");
    for (tgt, _) in &transforms_to_zero {
        println!("\t{}->0: {}", tgt, transform_to_zero(&transforms_to_zero, &vec![Point{x:0,y:0,z:0}], *tgt)[0]);
    }

    // convert all points into a monolithic cloud
    let mut cloud = Vec::new();
    for i in 0..scanners.len() {
        let mut subcloud = transform_to_zero(&transforms_to_zero, &scanners[i], i);
        // println!("Scanner {}:", i);
        // for pt in &subcloud {
        //     println!("\t{}", pt);
        // }
        cloud.append(&mut subcloud);
    }

    // count elements in cloud
    let mut counter: HashMap<Point, u64> = HashMap::new();
    for point in &cloud {
        *counter.entry(*point).or_insert(0) += 1;
    }
    println!("Part A: Found {} unique points out of {}.", counter.len(), cloud.len());


}










