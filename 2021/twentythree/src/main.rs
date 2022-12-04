// Path Planning for Amphipods
// https://adventofcode.com/2021/day/23

use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

// structure containing the current state of all amphipods
#[derive(Clone,Eq,PartialEq,Hash)]
struct AmphipodState {
    scale: u64,
    goal: u8,
    loc: Vec<(u8,u8)>,
    char_: char
}
type State = Vec<AmphipodState>;

// check if this state is a final state
fn finished(state: &State) -> bool {
    for val in state {
        for loc in &val.loc {
            if loc.0 != val.goal {
                return false;
            }
        }
    }
    return true;
}

// check if the target is occupied in the current state
fn is_open(state: &State, target: (u8, u8)) -> bool {
    for val in state {
        for loc in &val.loc {
            if *loc == target {
                return false;
            }
        }
    }
    return true;
}

// determine all possible (single amphipod) movements
//  from the given starting state, as well as associated cost
fn moves(state: &State, map: &HashMap<String,usize>) -> Vec<(u64,State)> {
    // initialize result
    let mut states = Vec::new();

    // then, find all possible moves for each ambiphod
    for (state_idx,val) in state.iter().enumerate() {
        for loc_idx in 0..val.loc.len() {
            states.append(&mut all_moves(state, &state_idx, &loc_idx));
        }
    }

    // return
    return states;
}

// return all valid next states for the given amphipod from the given state
fn all_moves(state: &State, state_idx: &usize, loc_idx: &usize) -> Vec<(u64,State)> {
    // initialize results
    let mut states = Vec::new();

    // check if we should do nothing - i.e. we're in the goal spot 
    // and all spots below us are siblings
    let my_loc = state[*state_idx].loc[*loc_idx];
    if my_loc.0 == state[*state_idx].goal && my_loc.1 > 0 {
        let mut stop = true;
        for idx in (my_loc.1+1)..state[*state_idx].loc.len()
    }

    // useful variable -> bool indicating if sibling occupies the goal
    let all_siblings_done = true;
    for idx in 0..state[*state_idx].loc.len() {
        siblings_done &= state[*state_idx].loc[idx].0 == state[*state_idx].goal;
    }

    // check if this pod (and sibling) are done
    if all_siblings_done {
        return states; 
    }

    // // otherwise we should move. Find all valid locations
    // let mut targets: Vec<(u8,u8)> = Vec::new();

    // // check if we can move to our goal location
    // if is_open(state, state[*idx].goal) {
    //     targets.push(state[*idx].goal);
    //     // try to navigate to goal
    // } else if sibling_done {
    //     // try to navigate to subgoal
    //     targets.push(state[*idx].subgoal);
    // }

    // // if we're not in the hallway we can also go to the hallway
    // if state[*idx].loc.0 != 0 {
    //     // push all valid targets
    //     targets.append(&mut vec![(0,0),(1,0),(3,0),(5,0),(7,0),(9,0),(10,0)]);
    // }

    // // create a state from each target, if it's reachable
    // for target in targets {
    //     let motions = moves_to_go(state, state[*idx].loc, target);
    //     if motions.is_some() {
    //         let cost = motions.unwrap() * state[*idx].scale;
    //         let mut new_state = state.clone();
    //         new_state[*idx].loc = target;
    //         states.push((cost,new_state));
    //     }
    // }
    return states;
}

// // return the number of moves to get to the desired position, if it's not blocked
// fn moves_to_go(state: &State, start: (u8,u8), target: (u8,u8)) -> Option<u64> {
//     // exit early options and sanity checks
//     assert!(!is_open(state, start));
//     if !is_open(state, target) {
//         return None;
//     }
// 
//     // initialize variables
//     let mut moves = 0;
//     let mut current = start.clone();
//     loop {
//         // check stop conditions
//         if current == target {
//             return Some(moves);
//         }
// 
//         // determine next move
//         let mut next = (0,0);
//         if (current.0 != target.0) && current.1 > 0 {
//             // we need to move out of this side room
//             next = (current.0, current.1 - 1);
//         } else if current.0 == target.0 {
//             // we're in the right side room, need to move down
//             next = (current.0, current.1 + 1);
//         } else if current.0 > target.0 {
//             // need to move left
//             next = (current.0 - 1, current.1);
//         } else {
//             // need to move right
//             next = (current.0 + 1, current.1);
//         }
//         assert!(next.0 <= 10);
//         assert!(next.1 <= 2);
// 
//         // if we made it this far, we've moved. Check if it was valid
//         if !is_open(state, next) {
//             return None;
//         }
//         // otherwise continue to next loop
//         moves += 1;
//         current = next;
//     }
// }

fn get_hash(state: &State) -> u64 {
    let mut s = DefaultHasher::new();
    state.hash(&mut s);
    return s.finish();
}

// iterate through all path combinations
fn find_shortest_path(start: &State, map: &HashMap<String,usize>) -> u64 {

    // keep track of all current states
    let mut states: Vec<State> = Vec::new();
    states.push(start.clone());

    // initialize list of finished states
    let mut cost_to_go: HashMap<u64,u64> = HashMap::new();
    cost_to_go.insert(get_hash(start),0);

    // track successful states
    let mut successful: Vec<(u64,State)> = Vec::new();
    let mut min = u64::MAX;

    loop {
        // set up a new vector for new states
        let mut new_states: Vec<State> = Vec::new();

        // loop through all current states
        for state in states {
            // get the cost top go to this state
            let cost = cost_to_go[&get_hash(&state)];

            // check if this state is done
            if finished(&state) {
                println!("Found successful state:");
                print_state(&state);
                successful.push((cost,state));
                if cost < min {
                    min = cost;
                    println!("Found new minimum cost: {}", min);
                }
                continue;
            }
            
            // println!("Evaluating the following state:");
            // print_state(&state);

            // otherwise, find all next possible states
            for (new_cost, new_state) in moves(&state, map) {
                // check if this new_state was already found, and update cost
                let new_hash = get_hash(&new_state);
                if cost_to_go.contains_key(&new_hash) {
                    if cost_to_go[&new_hash] > new_cost + cost {
                        *cost_to_go.get_mut(&new_hash).unwrap() = new_cost + cost;
                    }
                } else {
                    // this is new, insert it and add it to the list of states to pursue
                    cost_to_go.insert(new_hash, cost + new_cost);
                    new_states.push(new_state);
                }
            }
        }

        // update with new states, if any
        if new_states.len() == 0 {
            break;
        } else {
            println!("Found {} new states", new_states.len());
        }
        states = new_states;
    }
    return min;
}

fn print_state(state: &State) {
    let mut template: Vec<Vec<char>> = vec![
        vec!['#','#','#','#','#','#','#','#','#','#','#','#','#'],
        vec!['#','.','.','.','.','.','.','.','.','.','.','.','#'],
        vec!['#','#','#','.','#','.','#','.','#','.','#','#','#'],
        vec![' ',' ','#','.','#','.','#','.','#','.','#',' ',' '],
        vec![' ',' ','#','.','#','.','#','.','#','.','#',' ',' '],
        vec![' ',' ','#','.','#','.','#','.','#','.','#',' ',' '],
        vec![' ',' ','#','#','#','#','#','#','#','#','#',' ',' ']];
    for val in state.iter() {
        for loc in &val.loc {
            template[(loc.1 + 1) as usize][(loc.0 + 1) as usize] = val.char_;
        }
    }

    for i in 0..template.len() {
        let mut row = String::new();
        for j in 0..template[0].len() {
            row.push(template[i][j]);
        }
        println!("{:?}", row);
    }
}

fn main() {
    // hardcoded starting state

    // example 
    let start: State = vec![
        AmphipodState { scale: 1, goal: 2, loc: vec![(2,4),(6,3),(8,2),(8,4)], char_: 'A' },
        AmphipodState { scale: 10, goal: 4, loc: vec![(2,1),(4,3),(6,1),(6,2)], char_: 'B' },
        AmphipodState { scale: 100, goal: 6, loc: vec![(4,1),(4,2),(6,4),(8,3)], char_: 'C' },
        AmphipodState { scale: 1000, goal: 8, loc: vec![(2,2),(2,3),(4,4),(8,1)], char_: 'D' },
    ];
    // // actual input
    // let start: State = vec![
    //     AmphipodState { scale: 1, goal: (2,2), subgoal: (2,1), loc: (4,1), sibling: "A2".to_string(), char_: 'A' },
    //     AmphipodState { scale: 1, goal: (2,2), subgoal: (2,1), loc: (4,2), sibling: "A1".to_string(), char_: 'A' },
    //     AmphipodState { scale: 10, goal: (4,2), subgoal: (4,1), loc: (2,2), sibling: "B2".to_string(), char_: 'B' },
    //     AmphipodState { scale: 10, goal: (4,2), subgoal: (4,1), loc: (6,1), sibling: "B1".to_string(), char_: 'B' },
    //     AmphipodState { scale: 100, goal: (6,2), subgoal: (6,1), loc: (2,1), sibling: "C2".to_string(), char_: 'C' },
    //     AmphipodState { scale: 100, goal: (6,2), subgoal: (6,1), loc: (8,2), sibling: "C1".to_string(), char_: 'C' },
    //     AmphipodState { scale: 1000, goal: (8,2), subgoal: (8,1), loc: (6,2), sibling: "D2".to_string(), char_: 'D' },
    //     AmphipodState { scale: 1000, goal: (8,2), subgoal: (8,1), loc: (8,1), sibling: "D1".to_string(), char_: 'D' }
    // ];

    // define an index mapping
    let mut map: HashMap<String,usize> = HashMap::new();
    map.insert("A".to_string(),0);
    map.insert("B".to_string(),1);
    map.insert("C".to_string(),2);
    map.insert("D".to_string(),3);

    println!("Start state:");
    print_state(&start);

    // search across all states
    let min_cost = find_shortest_path(&start, &map);
    println!("Part A minimum cost: {}", min_cost);
}












