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
  let (mut count, mut prev) : (usize, usize) = (0, 0);
  for (index, line) in reader.lines().enumerate()
  {
    // extract value of line and convert to integer
    let line = line.expect("Unable to read line");
    let value : usize = line.parse().expect("Unable to parse line.");

    // skip first line
    if (index != 1) && (value > prev)
    {
      count += 1;
    }
    // track previous line
    prev = value
  }

  println!("Found {} increasing measurments.", count);
}
