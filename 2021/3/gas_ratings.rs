// analyze the given diagnostic data to determine power consumption
// https://adventofcode.com/2021/day/3

use std::vec::Vec;
use std::fs::File;
use std::io::{BufRead, BufReader};

// const FILENAME : &str = "example.txt";
const FILENAME : &str = "diagnostic.txt";

fn filter_by_index(report: &mut Vec::<u32>, index : u32, num_bits : u32, most : bool)
{
  // downsample the given report, dropping all elements whose values
  //  at the given index are either the most or least common
  let size = report.len() as u32;

  // exit early if we've just got one element
  if size == 1
  {
    return;
  }

  // first, iterate through and determine the most common bit value for the index
  let mut most_common_val = 0;
  for i in 0..report.len()
  {
    // get the value at the given index (bitshift and check remainder)
    let bit = (report[i] >> (num_bits - index - 1)) % 2;
    // println!("Val {} ({:b}) is {} at index {}", report[i], report[i], bit, index);
    
    most_common_val += bit;
  }

  // determine the most common val - this requires special handling for the cases where
  //  we have the same number of 1s and 0s, based on the problem definition.
  if ( size % 2 == 0 ) && ( most_common_val == size / 2 )
  {
    // edge case - assign value based on our filter type
    //  this actually ends up being the same value, since for the keep_most situation we
    //  want to retain 1, and for the keep_least we want to keep 0
    most_common_val = 1;
  }
  else
  {
    // println!("{} > {} : {}", most_common_val, size / 2, (most_common_val > size / 2));
    most_common_val = if most_common_val > size / 2 { 1 } else { 0 };
  }

  // println!("Most common value for index {} is {} (size: {})", index, most_common_val, size);

  // filter out points that don't match this value at this index
  if most
  {
    report.retain(|&x| ( (x >> (num_bits - index - 1)) % 2 == most_common_val));
  }
  else
  {
    report.retain(|&x| ( (x >> (num_bits - index - 1)) % 2 != most_common_val));
  }

  // println!("Report size is now {} for index {}/{} (keeping most: {})", report.len(), index, num_bits - 1, most);
  // for i in 0..report.len()
  // {
  //   println!("\t{:b}", report[i]);
  // }
}

fn main()
{
  // initialize result data and some intermediate variables
  let (mut o2_report, mut co2_report) = (Vec::<u32>::new(), Vec::<u32>::new());
  let mut num_bits = 0;

  // set up buffered file reader
  let file = File::open(FILENAME).expect("Couldn't read given file!.");
  let reader = BufReader::new(file);

  // iterate through the attached file and see where we end up
  for (index, line) in reader.lines().enumerate()
  {
    // make sure we could read this line
    let line = line.expect("Unable to read line.");

    // one time initialization
    if index == 0
    {
      num_bits = line.chars().count();
    }

    // convert value to decimal representation and add to our reports
    let val = u32::from_str_radix(&line, 2).expect("Unable to convert to decimal!");

    o2_report.push(val);
    co2_report.push(val);
  }

  // progressively filter each report until we (hopefully) have one value left
  for i in 0..num_bits
  {
    filter_by_index(&mut o2_report, i as u32, num_bits as u32, true);
    filter_by_index(&mut co2_report, i as u32, num_bits as u32, false);
  }

  assert_eq!(o2_report.len(), 1);
  assert_eq!(co2_report.len(), 1);

  // extract values themselves
  let o2 = o2_report[0];
  let co2 = co2_report[0];
  println!("Got (O2, CO2) ratings: ({}, {}) -> life rating = {}", o2, co2, o2 * co2);

}
