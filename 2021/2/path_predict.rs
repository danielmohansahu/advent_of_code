// Predict the final horizontal / vertical position of the given commands.
//

use std::fs::File;
use std::io::{BufRead, BufReader};

fn main()
{
  // hardcoded input file
  let filename = "commands.txt";

  // initialize results
  let (mut horizontal, mut vertical) : (usize, usize) = (0, 0);

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
      horizontal += distance;
    }
    else if direction == "down"
    {
      vertical += distance;
    }
    else if direction == "up"
    {
      vertical -= distance;
    }
    else
    {
      panic!("Invalid direction {}", direction);
    }
  }

  println!("Got displacement ({}, {}) - multiplied is {}", horizontal, vertical, horizontal * vertical);

}
