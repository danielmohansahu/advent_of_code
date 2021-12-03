// analyze the given diagnostic data to determine power consumption
// https://adventofcode.com/2021/day/3

use std::vec::Vec;
use std::fs::File;
use std::io::{BufRead, BufReader};

// const FILENAME : &str = "example.txt";
const FILENAME : &str = "diagnostic.txt";

fn main()
{
  // initialize results
  let mut count : u32 = 0;
  let mut vec = Vec::<u32>::new();

  // set up buffered file reader
  let file = File::open(FILENAME).expect("Couldn't read given file!.");
  let reader = BufReader::new(file);

  // iterate through the attached file and see where we end up
  for (index, line) in reader.lines().enumerate()
  {
    // make sure we could read this line
    let line = line.expect("Unable to read line.");

    // handle vector initialization for the first line
    if index == 0
    {
      let size = line.chars().count();
      for _ in 0..size
      {
        vec.push(0);
      }
    }

    // add this line of bits to our running tally
    for (idx, bit) in line.chars().enumerate()
    {
      let bit = bit.to_digit(10).expect("Unable to parse bit!");
      vec[idx] += bit;
    }

    // track the number of total lines
    count += 1;
  }

  // extract least and most significant bits
  let (mut epsilon, mut gamma) : (u32, u32) = (0, 0);
  for i in 0..vec.len()
  {
    // find the most and least common bit of this element
    let g = if vec[i] > (count / 2) { 1 } else { 0 };
    let e = if g == 1 { 0 } else { 1 }; 

    // add these values to our running counts
    let shift : i32 = (vec.len() - i) as i32 - 2;
    let binary_val = if shift > 0 { 2 << shift } else { 2 >> shift.abs() };
    gamma += g * binary_val;
    epsilon += e * binary_val;

    // println!("(g,e): ({}, {}); bval: {}", g, e, binary_val);
  }

  println!("Got (g, e) values of ({}, {}) -> fuel consumption = {}", gamma, epsilon, gamma * epsilon);

}
