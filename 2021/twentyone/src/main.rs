// I'm a bit worried about the Dirac reference...
// https://adventofcode.com/2021/day/21

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
    println!("\t die total: {}", die);

    // advance the piece
    state.space += (die % 10) as u8;
    while state.space > 10 {
        state.space -= 10;
    }
    println!("\t new space: {}", state.space);

    // add score and check for win condition
    state.score += state.space as u64;
    println!("\t new score: {}", state.score);
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
        println!("Player 1's turn:");
        p1_state.die = p2_state.die;
        roll_count += 3; 
        if turn(&mut p1_state) {
            println!("Part A Player 1 won! Score = {} * {} = {}", p2_state.score, roll_count, p2_state.score * roll_count);
            break;
        }

        println!("Player 2's turn:");
        p2_state.die = p1_state.die;
        roll_count += 3; 
        if turn(&mut p2_state) {
            println!("Part A Player 2 won! Score = {} * {} = {}", p1_state.score, roll_count, p1_state.score * roll_count);
            break;
        }
    }
}

fn main() {
    // play Part A's game
    game();
}
