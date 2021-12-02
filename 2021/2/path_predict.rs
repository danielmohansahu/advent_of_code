// Predict the final horizontal / vertical position of the given commands.
//

use std::fs::File;
use std::io::{BufRead, BufReader};

fn main()
{
  // hardcoded input file
  let filename = "commands.txt";

  // initialize results
  let (mut h1, mut h2, mut v1, mut v2, mut aim) : (usize, usize, usize, usize, usize) = (0, 0, 0, 0, 0);

  // set up buffered file reader
  let file = File::open(filename).expect("Couldn't read given file!.");
  let reader = BufReader::new(file);

  // iterate through the attached file and see where we end up
  for line in reader.lines()
  {
    // read line
    let line = line.expect("Unable to read line");
    let mut words = line.split_whitespace();
    let direction = words.next().expect("Didn't find a direction.");
    let distance : usize = words.next().expect("Unable to find a distance.").parse().expect("Unable to parse.");

    if direction == "forward"
    {
      h1 += distance;
      h2 += distance;
      v2 += aim * distance;
    }
    else if direction == "down"
    {
      aim += distance;
      v1 += distance;
    }
    else if direction == "up"
    {
      aim -= distance;
      v1 -= distance;
    }
    else
    {
      panic!("Invalid direction {}", direction);
    }
  }

  println!("Part 1 got displacement ({}, {}) - multiplied is {}", h1, v1, h1 * v1);
  println!("Part 2 got displacement ({}, {}) - multiplied is {}", h2, v2, h2 * v2);

}
