// I'm a bit worried about the Dirac reference...
// https://adventofcode.com/2021/day/21

use std::fmt;
use std::collections::HashMap;

// example inputs
// static P1_start: u8 = 4;
// static P2_start: u8 = 8;

// real inputs
static P1_start: u8 = 3;
static P2_start: u8 = 4;

struct State {
    space: u8,
    die: u8,
    score: u64
}

fn turn(state: &mut State) -> bool{
    // play one round of the game for the given player's state
    //  return true if the player wins

    // roll the die 3 times
    let mut die: u32 = 0;
    for _ in 0..3 {
        die += state.die as u32;
        state.die += 1;
        if state.die == 101 {
            state.die = 1;
        }
    }

    // advance the piece
    state.space += (die % 10) as u8;
    while state.space > 10 {
        state.space -= 10;
    }

    // add score and check for win condition
    state.score += state.space as u64;
    return state.score > 999;
}

fn game() {
    // play a single game (PartA) and return final score
    
    // initialize states
    let (mut p1_state, mut p2_state) = (
        State {space: P1_start, die: 1, score: 0},
        State {space: P2_start, die: 1, score: 0});

    // loop until done
    let mut roll_count = 0;
    loop {
        p1_state.die = p2_state.die;
        roll_count += 3; 
        if turn(&mut p1_state) {
            println!("Part A Player 1 won! Score = {} * {} = {}", p2_state.score, roll_count, p2_state.score * roll_count);
            break;
        }

        roll_count += 3; 
        if turn(&mut p2_state) {
            println!("Part A Player 2 won! Score = {} * {} = {}", p1_state.score, roll_count, p1_state.score * roll_count);
            break;
        }
    }
}

#[derive(Copy, Clone, Debug, Hash)]
struct Universe {
    p1: u8,     // player 1 space
    p2: u8,     // player 2 space
    s1: u64,    // player 1 score
    s2: u64     // player 2 score
}

// display for my universe, for convenience
impl fmt::Display for Universe {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({},{},{},{})", self.p1, self.p2, self.s1, self.s2)
    }
}

// equality comparison for Universe
impl PartialEq for Universe {
    fn eq(&self, other: &Self) -> bool {
        self.p1 == other.p1 && self.p2 == other.p2 && self.s1 == other.s1 && self.s2 == other.s2
    }
}
impl Eq for Universe {}

fn advance_p1(universe: &mut Universe, roll: u8) -> bool {
    // advance the given universe, returning true if both players won

    // advance the piece
    universe.p1 += (roll % 10) as u8;
    while universe.p1 > 10 {
        universe.p1 -= 10;
    }

    // add score and check for win condition
    universe.s1 += universe.p1 as u64;
    return universe.s1 > 20
}

fn advance_p2(universe: &mut Universe, roll: u8) -> bool {
    // advance the given universe, returning true if both players won

    // advance the piece
    universe.p2 += (roll % 10) as u8;
    while universe.p2 > 10 {
        universe.p2 -= 10;
    }

    // add score and check for win condition
    universe.s2 += universe.p2 as u64;
    return universe.s2 > 20;
}

fn quantum_game() {
    // pre-computed set of all possible dice rolls, in order
    let rolls: Vec<u8> = vec![3,4,5,4,5,6,5,6,7,4,5,6,5,6,7,6,7,8,5,6,7,6,7,8,7,8,9];

    // play a quantum game
    let mut universes: HashMap<Universe,u64> = HashMap::new();
    universes.insert(Universe {p1: P1_start, p2: P2_start, s1: 0, s2: 0}, 1);

    // loop until all possibilities are exhausted
    let (mut p1_count, mut p2_count) = (0, 0);
    while universes.len() != 0 {
        let mut newverses: HashMap<Universe, u64> = HashMap::new();
        for (universe, count) in universes {
            // println!("Testing {} universes: {}", count, universe);
            // advance this universe once for each possible die permutation (rolled thrice)
            for r1 in &rolls {
                // copy this universe as a starting point
                let mut new = universe.clone();

                // check if player one wins
                if advance_p1(&mut new, *r1) {
                    // this universe is done, tally
                    p1_count += count;
                    continue;
                }

                // let player 2 play
                for r2 in &rolls {
                    // take the end of player 1's turn as the starting point
                    let mut new2 = new.clone();

                    // check if player2 won
                    if advance_p2(&mut new2, *r2) {
                        p2_count += count;
                    } else {
                        // nobody has won in this universe, add it for the next iteration
                        *newverses.entry(new2).or_insert(0) += count;
                    }
                }
            }
        }
        // update total list of universes for the next iteration, e.g. to drop finished ones
        universes = newverses;
    }

    // total win counts:
    println!("Part B");
    if p1_count > p2_count {
        println!("\tPlayer 1 wins: {}", p1_count);
    } else {
        println!("\tPlayer 2 wins: {}", p2_count);
    }
}


fn main() {
    // play Part A's game
    game();

    // play Part B's game
    quantum_game();
}
