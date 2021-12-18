// Helping Snailfish with Homework
// https://adventofcode.com/2021/day/18

use std::vec::Vec;
use either::{Either,Left,Right};
use std::fs::File;
use std::io::{BufRead, BufReader};
use anyhow::Result;

// static INPUT: &str = "simple_example1.txt";
// static INPUT: &str = "simple_example2.txt";
// static INPUT: &str = "simple_example3.txt";
// static INPUT: &str = "simple_example4.txt";
// static INPUT: &str = "example1.txt";
// static INPUT: &str = "example2.txt";
static INPUT: &str = "homework.txt";

// struct that can store itself or a regular number as a pair
struct RecursivePair {
    left: Either<u32,Box<RecursivePair>>,
    right: Either<u32,Box<RecursivePair>>
}

fn get_substrings(line: &str) -> (&str,&str) {
    // we assume the structure is "[ A , B ]"
    // sanity checks
    assert_eq!(line.chars().next().unwrap(), '[');
    assert_eq!(line.chars().last().unwrap(), ']');

    // get the first and second top level pair elements
    // basically, we're looking for the first string delimeter encountered
    // with an open parantheses count of 1
    let mut count = 0;
    let mut split_idx = 0;
    for (i,char_) in line.chars().enumerate() {
        match char_ {
            '[' => count += 1,
            ']' => count -= 1,
            ',' => {
                // if count == 1, this is our stop condition
                if count == 1 {
                    split_idx = i;
                    break;
                }
            },
            _   => ()
        }
    }
    
    // return our two substrings
    // get a subline, ignoring the open and close brackets
    let subline = &line[1..line.chars().count()-1];
    split_idx -= 1; // to account for the change in line size

    // this split only works because we know this is ASCII - otherwise it would fail
    let (left, mut right) = subline.split_at(split_idx);
    right = &right[1..]; // get rid of the ','
    
    // println!("Split {:?} into {:?} and {:?}", line, left, right);
    return (left,right);
}

// parse a single line (String) into a Snailfish number
fn parse_line(line: &str) -> RecursivePair {
    // get left and right substrings
    let (left,right) = get_substrings(line);

    // construct result based on whether or not these are regular numbers
    // get the first character of each for convenience
    let (c_l, c_r) = (left.chars().next().unwrap(), right.chars().next().unwrap());
    RecursivePair {
        left:  if c_l == '[' { Right(Box::new(parse_line(left)))  } else { Left(c_l.to_digit(10).unwrap()) },
        right: if c_r == '[' { Right(Box::new(parse_line(right))) } else { Left(c_r.to_digit(10).unwrap()) }
    }
}

// parse the given input file into a vector of snailfish numbers
fn parse_input(filename: &str) -> Result<Vec<RecursivePair>> {
    // parse input into a vector of lines
    let mut lines: Vec<RecursivePair> = Vec::new();

    // set up a reader and file object
    let file = File::open(filename)?;
    let reader = BufReader::new(file);

    // parse all the lines into our various arrays
    for line in reader.lines() {
        let line = line?;
        lines.push(parse_line(&line));
    }

    Ok(lines)
}

// debugging function to print custom struct
fn to_string(pair: &RecursivePair) -> String {
    // initialize result
    let mut string = String::from("[");
    
    // check if first element is a number
    if pair.left.is_left() {
        string.push_str(&pair.left.as_ref().left().unwrap().to_string());
    } else {
        string.push_str(&to_string(pair.left.as_ref().right().unwrap()));
    }

    // add delimiter
    string.push(',');

    // add second element
    if pair.right.is_left() {
        string.push_str(&pair.right.as_ref().left().unwrap().to_string());
    } else {
        string.push_str(&to_string(pair.right.as_ref().right().unwrap()));
    }
    
    string.push(']');
    return string;
}

// walk (right child) down the given subtree, adding the given value to the first (left) regular
// value encountered
fn add_to_first_left(number: &mut Either<u32,Box<RecursivePair>>, value: u32) {
    // check if this is a regular number, in which case we can just add it
    if number.is_left() {
        *number.as_mut().left().unwrap() += value;
    } else {
        // continue searching
        add_to_first_left(&mut number.as_mut().right().unwrap().left, value);
    }
}

// walk (right child) down the given subtree, adding the given value to the first (left) regular
// value encountered
fn add_to_first_right(number: &mut Either<u32,Box<RecursivePair>>, value: u32) {
    // check if this is a regular number, in which case we can just add it
    if number.is_left() {
        *number.as_mut().left().unwrap() += value;
    } else {
        // continue searching (to the left)
        add_to_first_right(&mut number.as_mut().right().unwrap().right, value);
    }
}

// recurse through the structure, checking if any children should explode
fn explode(pair: &mut Either<u32,Box<RecursivePair>>, level: u8, done: &mut bool) -> Option<(u32,u32)> {
    // if we're a regular value, continue
    if pair.is_left() {
        return None;
    }
    // get reference to the recursive pair
    let number = pair.as_mut().right().unwrap();
    // println!("\texploding level {}: {:?}", level, to_string(&number));

    // check if we should explode (assuming nobody already did)
    if !*done && level > 3 {
        // sanity check that our children are regular values
        assert!(number.left.is_left());
        assert!(number.right.is_left());
        let res = (*number.left.as_ref().left().unwrap(), *number.right.as_ref().left().unwrap());

        println!("\texploding {:?}", to_string(&number));
        // set our value to 0
        *pair = Left(0);
        *done = true;
        return Some(res);
    }

    // check if left option exploded
    let lvals = explode(&mut number.left, level + 1, done);
    let rvals = explode(&mut number.right, level + 1, done);

    // return early if we've gotten nothing
    if lvals.is_none() && rvals.is_none() {
        return None;
    }

    // check if the child nodes just exploded
    if level == 3 {
        // check if left node exploded
        if lvals.is_some() {
            // add the right lval to the right child
            let (l,r) = lvals.unwrap();
            add_to_first_left(&mut number.right, r);
            return Some((l,0));
        } else {
            // add the left rval to the left child
            let (l,r) = rvals.unwrap();
            add_to_first_right(&mut number.left, l);
            return Some((0,r));
        }
    } else {
        // we're passing information back upwards.
        if lvals.is_some() {
            let (l,r) = lvals.unwrap();
            if l != 0 && r != 0 {
                panic!("Unexpected situation.");
            } else if l != 0 {
                // our left child is passing a left upwards - keep it going
                return Some((l,0));
            } else {
                // our left child is passing a right upwards - pass it to our right
                add_to_first_left(&mut number.right, r);
            }
        } else {
            let (l,r) = rvals.unwrap();
            if l != 0 && r != 0 {
                panic!("Unexpected situation.");
            } else if l != 0 {
                // our right child is passing a left upwards - pass it to our left
                // println!("Before add to first right:\n{:?}", to_string(&number));
                add_to_first_right(&mut number.left, l);
                // println!("After add to first right:\n{:?}", to_string(&number));
            } else {
                // our right child is passing a right upwards - keep it going
                return Some((0,r));
            }
        }
    }

    // we didn't explode, sorry
    None
    
}

// recurse through the structure, splitting if necessary
fn split(number: &mut RecursivePair, done: &mut bool) {
    // check if we're done, in which case we should immediately return
    if *done {
        return;
    }

    // check left
    if number.left.is_left() {
        let val = number.left.as_ref().left().unwrap();
        if val > &9 {
            let val_l = if val % 2 == 1 { (val - 1) / 2 } else { val / 2 };
            let val_r = if val % 2 == 1 { (val + 1) / 2 } else { val / 2 };
            assert_eq!(val_l + val_r, *val);
            println!("\tsplitting {} in {:?} into ({},{})", val, to_string(&number), val_l, val_r);
            number.left = Right(Box::new(RecursivePair{left: Left(val_l), right: Left(val_r)}));
            *done = true;
            return;
        }
    } else {
        // recurse
        split(&mut number.left.as_mut().right().unwrap(), done);
    }
    
    if *done {
        return;
    }

    // check right
    if number.right.is_left() {
        let val = number.right.as_ref().left().unwrap();
        if val > &9 {
            let val_l = if val % 2 == 1 { (val - 1) / 2 } else { val / 2 };
            let val_r = if val % 2 == 1 { (val + 1) / 2 } else { val / 2 };
            assert_eq!(val_l + val_r, *val);
            println!("\tsplitting {} in {:?} into ({},{})", val, to_string(&number), val_l, val_r);
            number.right = Right(Box::new(RecursivePair{left: Left(val_l), right: Left(val_r)}));
            *done = true;
            return;
        }
    } else {
        // recurse
        split(&mut number.right.as_mut().right().unwrap(), done);
    }
}

fn reduce(mut sum: RecursivePair) -> RecursivePair {
    // apply our reduction operations, one action at a time
    // our string representation, used for tracking when we finish
    let mut repr = to_string(&sum);

    println!("  reducing {:?}", repr);

    // convenience variable used to track when to reset
    let mut done = false;
    loop {
        // reset variable
        done = false;

        // first try to explode (once)
        let mut tmp: Either<u32,Box<RecursivePair>> = Right(Box::new(sum));
        explode(&mut tmp, 0, &mut done);
        sum = *tmp.right().unwrap();
        if done {
            println!("    after explode: \n\t{:?}", to_string(&sum));
            continue;
        }

        // then try to split
        split(&mut sum, &mut done);
        println!("    after split: \n\t{:?}", to_string(&sum));

        // check if we didn't do anything, in which case we should stop
        let new_repr = to_string(&sum);
        if new_repr == repr {
            break;
        }
        repr = new_repr;
    }
    sum

}

// add numbers
fn add(lhs: RecursivePair, rhs: RecursivePair) -> RecursivePair {
    RecursivePair {left: Right(Box::new(lhs)), right: Right(Box::new(rhs)) }
}

// calculate the magnitude of a given number
fn magnitude(number: &RecursivePair) -> u32 {
    // check if subelements are regular vectors
    let mut result = 0;
    if number.left.is_left() {
        result += 3 * number.left.as_ref().left().unwrap();
    } else {
        result += 3 * magnitude(number.left.as_ref().right().unwrap());
    }
    if number.right.is_left() {
        result += 2 * number.right.as_ref().left().unwrap();
    } else {
        result += 2 * magnitude(number.right.as_ref().right().unwrap());
    }
    return result;
}

fn main() {
    // parse input
    let numbers = parse_input(INPUT).unwrap();

    // add numbers to get a result
    println!("First number: \n\t{:?}", to_string(&numbers[0]));
    let mut sum = RecursivePair {left: Left(0), right: Left(0)};
    let mut count = 0;
    for number in numbers {
        count += 1;

        // if this is the first number, just use it directly
        if count == 1 {
            sum = number;
            continue;
        }

        println!("Adding: \n  {:?}\n + {:?}", to_string(&sum), to_string(&number));
        
        sum = add(sum, number);
        sum = reduce(sum);
        
        println!("After adding and reducing #{}: \n\t{:?}", count, to_string(&sum));
    }
    let mag = magnitude(&sum);
    println!("Part A: Resulting sum has a magnitude of {}", mag);
}







