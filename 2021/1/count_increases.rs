// count the number of sequential increases in the given input data
// https://adventofcode.com/2021/day/1

// use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader};

fn main()
{
  // hardcoded filename
  let filename = "measurements.txt";

  // set up buffered file reader
  let file = File::open(filename).expect("Couldn't read given file!.");
  let reader = BufReader::new(file);

  // iterate through text file, counting the number of increasing differences
  let (mut raw_count, mut sliding_count, mut prev, mut prev2, mut prev3) : (usize, usize, usize, usize, usize) = (0, 0, 0, 0, 0);
  for (index, line) in reader.lines().enumerate()
  {
    // extract value of line and convert to integer
    let line = line.expect("Unable to read line");
    let value : usize = line.parse().expect("Unable to parse line.");

    // skip first line when considering the raw increases count
    if (index != 0) && (value > prev)
    {
      raw_count += 1;
    }

    // skip first two lines when considering the sliding scale count
    if (index > 2) && ( (prev2 + prev + value) > (prev3 + prev2 + prev) )
    {
      println!("{} > {}, incrementing.", (prev2 + prev + value), (prev3 + prev2 + prev));
      sliding_count += 1;
    }

    // track previous lines
    prev3 = prev2;
    prev2 = prev;
    prev = value;
  }

  println!("Found {} increasing measurements.", raw_count);
  println!("Found {} increasing measurements with sliding scale filter.", sliding_count);
}
