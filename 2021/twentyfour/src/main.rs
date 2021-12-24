// Brute force MONAD hacking
// https://adventofcode.com/2021/day/24

static FILENAME: &str = "monad.txt";

use std::vec::Vec;
use anyhow::Result;
use std::fs::File;
use std::io::{BufRead, BufReader};

// All possible instructions
enum INSTRUCTION { INP, ADD, MUL, DIV, MOD, EQL }

// variables used in instruction set
enum VAR { W, X, Y, Z }

// possible values for the third input to instruction set
enum OP {
    CONST(i32),
    VARIABLE(VAR)
}

// convenience TypeDefs
type InstructionSet = Vec<(INSTRUCTION,VAR,Option<OP>)>;

// parse the given file into a set of commands
fn parse_input(filename: &str) -> Result<InstructionSet> {
    // initialize result
    let mut instructions: InstructionSet = Vec::new();

    // set up a reader and file object
    let file = File::open(filename)?;
    let reader = BufReader::new(file);

    // parse inputs
    for line in reader.lines() {
        // split into whitespace
        let line = line?;
        let mut split = line.split_whitespace();

        // get the input variables
        let cmd = split.next().unwrap();
        let var = match split.next().unwrap() {
            "w" => VAR::W,
            "x" => VAR::X,
            "y" => VAR::Y,
            "z" => VAR::Z,
            _   => panic!("Got unexpected variable in {:?}", line)
        };
        let other_var = split.next();       // not unwrapping yet - this is optional

        // check if this is an input instruction (the only one that doesn't have three inputs)
        if cmd == "inp" {
            instructions.push((INSTRUCTION::INP,var,None));
        } else {
            // get the third value, which can be an integer or VAR
            let other_var = other_var.unwrap();
            let val = match other_var {
                "w" => OP::VARIABLE(VAR::W),
                "x" => OP::VARIABLE(VAR::X),
                "y" => OP::VARIABLE(VAR::Y),
                "z" => OP::VARIABLE(VAR::Z),
                _   => OP::CONST(other_var.parse().unwrap())
            };

            // add a new instruction based on the given command
            instructions.push(match cmd {
                "add" => (INSTRUCTION::ADD, var, Some(val)),
                "mul" => (INSTRUCTION::MUL, var, Some(val)),
                "div" => (INSTRUCTION::DIV, var, Some(val)),
                "mod" => (INSTRUCTION::MOD, var, Some(val)),
                "eql" => (INSTRUCTION::EQL, var, Some(val)),
                _     => panic!("Unexpected command: {}", cmd)
            });
        }
    }

    Ok(instructions)
}

// execture the MONAD instruction set against the given integer
fn execute_monad(number: &Vec<u8>, instructions: &InstructionSet) -> bool {
    // sanity checks
    assert_eq!(number.len(), 14);

    // sequentially process the instruction set
    let mut idx = 0;
    for instruction in instructions {

    }
    false
}

// find the next lowest model number
fn decrement(number: &mut Vec<u8>) {
    // process in reverse, since we're big-endian (I think that's right?)
    for i in (0..number.len()).rev() {
        if number[i] > 1 {
            number[i] -= 1;
            return;
        } else {
            // skip 0, and continue to next digit
            number[i] = 9;
        }
    }
}

fn main() {
    // parse the input instruction for Part A
    let monad = parse_input(FILENAME).unwrap();
    
    // starting from the highest possible value, decrement until we get
    // to the first accepted value
    let mut model = vec![9,9,9,9,9,9,9,9,9,9,9,9,9,9];
    loop {
        println!("Checking model: {:?}", model);
        // check if this model is valid, breaking if so
        if execute_monad(&model, &monad) {
            println!("Found highest valid Model: {:?}", model);
            break;
        }
        // otherwise decrement and continue
        decrement(&mut model);
    }
}
