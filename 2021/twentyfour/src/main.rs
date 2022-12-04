// Brute force MONAD hacking
// https://adventofcode.com/2021/day/24

static FILENAME: &str = "monad.txt";

use std::vec::Vec;
use std::collections::HashMap;
use anyhow::Result;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

// All possible instructions
#[derive(Hash,PartialEq,Eq,Debug)]
enum INSTRUCTION { INP, ADD, MUL, DIV, MOD, EQL }

// variables used in instruction set
#[derive(Hash,PartialEq,Eq,Copy,Clone)]
enum VAR { W, X, Y, Z }

// possible values for the third input to instruction set
enum OP {
    CONST(i64),
    VARIABLE(VAR)
}

// convenience TypeDefs
type InstructionSet = Vec<(INSTRUCTION,VAR,Option<OP>)>;

// tracker to prevent re-searching already processed combinations of z->vector slice
type FoundSet = Vec<HashMap<(i64,u64),bool>>;

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
fn execute_monad(number: &Vec<u8>, instructions: &InstructionSet, found: &mut FoundSet) -> bool {
    // sanity checks
    assert_eq!(number.len(), 14);

    // initialize loop variables
    let mut idx = 0;
    let mut Q: HashMap<VAR,i64> = HashMap::new();
    Q.insert(VAR::W,0);
    Q.insert(VAR::X,0);
    Q.insert(VAR::Y,0);
    Q.insert(VAR::Z,0);

    // sequentially process the instruction set
    // let mut i = 1;
    // println!("Processing instruction set for {:?}", number);
    for (instruction,a,val) in instructions {
        // println!("\tprocessing instruction #{}: {:?}", i, instruction);
        // process input instructions
        if instruction == &INSTRUCTION::INP {
            // println!("\t  setting {:?} to {}", a, number[idx]);
            *Q.entry(*a).or_insert(0) = number[idx] as i64;

            // check if we've already processed this subset
            let mut s = DefaultHasher::new();
            number[idx..].hash(&mut s);
            let key = (Q[&VAR::Z],s.finish());
            if found[idx].contains_key(&key) {
                // println!("Found key : {:?}", key);
                return false;
            } else {
                // otherwise add this key
                // println!("Inserting key {:?}", key);
                // technically using 'false' here will also invalidate correct answers..
                found[idx].insert(key,false);
            }

            idx += 1;
        } else {
            // get the value of the optional variable
            let val = val.as_ref().unwrap();
            let b: i64 = match val {
                OP::CONST(val)    => *val,
                OP::VARIABLE(val) => Q[&val]
            };

            // println!("\t  (a,b) = ({:?},{})", a, b);
            
            // perform desired instruction
            *Q.entry(*a).or_insert(0) = match instruction {
                INSTRUCTION::ADD => Q[a] + b,
                INSTRUCTION::MUL => Q[a] * b,
                INSTRUCTION::DIV => {
                    assert!(b != 0);
                    Q[a] / b 
                },
                INSTRUCTION::MOD => {
                    assert!(b > 0);
                    assert!(Q[a] >= 0);
                    Q[a] % b
                },
                INSTRUCTION::EQL => if Q[a] == b {1} else {0},
                _ => panic!("Unreachable code.")
            }
        }
        // println!("\t  Q = ({},{},{},{})", Q[&VAR::W], Q[&VAR::X], Q[&VAR::Y], Q[&VAR::Z]);
        // i += 1;
    }

    // "valid" if z == 0
    return Q[&VAR::Z] == 0;
}

// find the next lowest model number
fn decrement(number: &mut Vec<u8>) {

    // loop until all conditions are satisified
    loop {
        // process in reverse, since we're big-endian (I think that's right?)
        for i in (0..number.len()).rev() {
            if number[i] > 1 {
                number[i] -= 1;
                break;
            } else {
                // skip 0, and continue to next digit
                number[i] = 9;
            }
        }
        if number[3] == number[4] + 3 
            && number[6] == number[7] + 2 
            && number[8] == number[9] + 5
            // && number[10] == number[11] + 6
            // && number[11] + 3 == number[12]
            // && number[10] == number[12]
            && number[12] == number[13] + 1 {
            break;
        }
    }
}

// find the next lowest model number
fn increment(number: &mut Vec<u8>) {
    // process in reverse, since we're big-endian (I think that's right?)
    for i in (0..number.len()).rev() {
        if number[i] < 9 {
            number[i] += 1;
            return;
        } else {
            // skip 0, and continue to next digit
            number[i] = 1;
        }
    }
}

fn manually_process(number: &Vec<u8>) -> bool {
    // step one can be simplified to the following
    let mut z = number[0] as i64 + 12;
    // step two simplified
    z = 26 * z + number[1] as i64 + 7;
    // step three simplified
    z = 26 * z + number[2] as i64 + 1;
    // step four simplified
    z = 26 * z + number[3] as i64 + 2;

    // step five (could go down or stay the same)
    assert_eq!(z % 26 - 5, number[4] as i64);
    z /= 26;

    // step six simplified
    z = 26 * z + number[5] as i64 + 15;
    // step seven simplified
    z = 26 * z + number[6] as i64 + 11;

    // step eight (could go down or stay the same)
    assert_eq!(z % 26 - 13, number[7] as i64);
    z /= 26;

    // step nine simplified
    z = 26 * (z / 26) + number[8] as i64 + 3;

    // step ten (could go down or stay the same)
    assert_eq!(z % 26 - 8, number[9] as i64);
    z /= 26;

    // step eleven simplified
    z = 26 * z + number[10] as i64 + 2;

    // step twelve (could go down or stay the same)
    assert_eq!(z % 26 - 8, number[11] as i64);
    z /= 26;

    // step thirteen (could go down or stay the same)
    assert_eq!(z % 26, number[12] as i64);
    z /= 26;

    // step thirteen (simplified)
    return z == 4 + number[13] as i64;
}

fn step(z: &mut i64, w: i64, x_off: i64, y_off: i64) -> bool {
    // manually simplified process - return true/false if decresed / stayed the same
    if (*z % 26 + x_off) == w {
        *z = *z / 26;
        return true;
    } else {
        *z = 26 * (*z / 26) + w + y_off;
        return false;
    };
}

fn main() {
    // parse the input instruction for Part A
    let monad = parse_input(FILENAME).unwrap();

    // initialize a giant map for tracking already solved subsets
    // let mut found = vec![HashMap::new(); 14];
    
    // starting from the highest possible value, decrement until we get
    // to the first accepted value
    let mut model = vec![9,9,9,9,9,9,9,9,9,9,9,9,9,9];
    let mut counter = 0;
    loop {
        // we only decrement at the top of the loop because we know the starting
        // point isn't right, or this would be an edge case bug
        decrement(&mut model);

        if counter % 1 == 0 {
            println!("Checking model: {:?}", model);
        }

        // check if this model is valid, breaking if so
        // if execute_monad(&model, &monad, &mut found) {
        if manually_process(&model) {
            println!("Found highest valid Model: {:?}", model);
            break;
        }
        counter += 1;
    }
   
    // // starting from the lowest possible value, increment until we find an invalid value
    // let mut model = vec![1,1,1,1,1,1,1,1,1,1,1,1,1,1];
    // loop {
    //     println!("Checking model: {:?}", model);
    //     // check if this model is valid, breaking if so
    //     if !execute_monad(&model, &monad) {
    //         // find last valid
    //         decrement(&mut model);
    //         println!("Found highest valid Model: {:?}", model);
    //         break;
    //     }
    //     // otherwise increment and continue
    //     increment(&mut model);
    // }

    // // debugging printouts
    // let mut model = vec![1,1,1,1,1,1,1,1,1,1,1,1,1,1];
    // loop {
    //     // check if this model is valid, breaking if so
    //     if execute_monad(&model, &monad) {
    //         println!("Found highest valid Model: {:?}", model);
    //     }
    //     increment(&mut model);
    // }
}
