// Brute force path planning (underwater)
// https://adventofcode.com/2021/day/12

use std::vec::Vec;
use std::collections::HashMap;
// use anyhow::Result;

// node structure - shows where each node is connected
struct Node {
    small: bool,
    connections: Vec<String>,
}

fn get_max_count(path: &Vec<String>) -> u32 {
    // return the maximum count of any element in the given vector
    // possibly the least efficient implementation of this possible?
    let mut tmp: HashMap<String,u32> = HashMap::new();
    for node in path {
        // only check nodes that are small
        if node == "start" || !node.chars().all(|c| c.is_ascii_lowercase()) {
            continue;
        }
        *tmp.entry(node.to_string()).or_insert(0) += 1;
    }

    // return the highest value
    let mut highest = 0;
    for kv in &tmp {
        if kv.1 > &highest {
            highest = *kv.1;
        }
    }

    // println!("{:?} : {:?}", path, tmp);

    return highest;
}

fn main() {
    // parse input into a dict of nodes
    let mut nodes: HashMap<String,Node> = HashMap::new();
    for line in include_str!("../edges.txt").lines() {
        // split into source and target
        let mut split = line.split('-');
        let src = split.next().unwrap();
        let tgt = split.next().unwrap();

        // add new nodes to map (if missing)
        if !nodes.contains_key(src) {
            nodes.insert(src.to_string(),
                         Node { small: src.chars().all(|c| c.is_ascii_lowercase()),
                                connections: Vec::new()});
        }
        if !nodes.contains_key(tgt) {
            nodes.insert(tgt.to_string(),
                         Node { small: tgt.chars().all(|c| c.is_ascii_lowercase()),
                                connections: Vec::new()});
        }

        // mark this edge for both nodes
        nodes.get_mut(src).unwrap().connections.push(tgt.to_string());
        nodes.get_mut(tgt).unwrap().connections.push(src.to_string());
    }

    // perform brute force search across all connections
    let mut final_paths: Vec<Vec<String>> = Vec::new();
    let mut current_paths: Vec<Vec<String>> = vec![vec!["start".to_string()]];

    loop {
        // initialize empty paths to add to the existing set
        let mut new_final_paths: Vec<Vec<String>> = Vec::new();
        let mut new_current_paths: Vec<Vec<String>> = Vec::new();

        // iterate through existing 'current paths'
        for path in &current_paths {
            // get the last node's connections
            for node in &nodes[path.last().unwrap()].connections {

                // Code for Part A:
                // // skip if this node is small and we've already seen it
                // if nodes[node].small && path.contains(node) {
                //     continue;
                // }
                
                // Code for Part B:
                // skip if this node is small and has been visited more than
                // twice
                if node == "start" {
                    continue;
                } else if nodes[node].small {
                    // check if the number of instances of this node are > 1
                    let count = path.iter().filter(|&e| *e == *node).count();
                    if count > 1 {
                        // already have 2, can't add more
                        continue;
                    } else if count == 1 {
                        // here's the tricky part - we can add this one _only_
                        // if there are no other nodes with 2 visits in this path
                        if get_max_count(&path) == 2 {
                            continue;
                        }
                        // println!("{:?} has 1 instance of {} and no others with 2 - adding it.", path, node);
                        // otherwise we can add this duplicate visit
                    }
                    // if count == 0 we're fine to go down to the next instruction and add it
                }

                // construct a full path with this node appended
                let mut new_path = path.clone();
                new_path.push(node.to_string());
                
                // check if this is a terminal node
                if node == "end" {
                    // add it to our final paths
                    new_final_paths.push(new_path);
                } else {
                    // add it to our intermediate paths
                    new_current_paths.push(new_path);
                }
                // println!("Checking if {:?} should add {}", path, node);
            }
        }

        // check stop condition - neither fineal_paths or current_paths have new nodes
        if (new_final_paths.len() == 0) && (new_current_paths.len() == 0) {
            // expanded all potential paths, stopping
            break;
        }

        // update our result structures
        for path in new_final_paths {
            final_paths.push(path);
        }
        current_paths = new_current_paths;
    }

    for path in &final_paths {
        println!("\t{:?}", path);
    }
    println!("Part A: Found {} total paths.", final_paths.len());
}



