// Image Dilation and Erosion, the Hard Way
// https://adventofcode.com/2021/day/20

use std::vec::Vec;
use std::collections::HashMap;
use anyhow::Result;
use std::fs::File;
use std::io::{BufRead, BufReader};

// static FILENAME: &str = "example.txt";
static FILENAME: &str = "image.txt";

fn parse_input(filename: &str) -> Result<(HashMap<usize,char>,Vec<Vec<char>>)> {
    // parse the given input file into a map of indices->char and 
    // the starting image

    // initialize results
    let mut map = HashMap::new();
    let mut image = Vec::new();

    // set up a reader and file object
    let file = File::open(filename)?;
    let reader = BufReader::new(file);

    // parse inputs
    for (i,line) in reader.lines().enumerate() {
        let line = line?;

        // the first line is the map line
        if i == 0 {
            for (j,char_) in line.chars().enumerate() {
                map.insert(j,char_);
            }
            continue;
        } else if i == 1 {
            // second line is an empty string; skip
            continue;
        }

        // populate image
        let mut row = Vec::new();
        for char_ in line.chars() {
            row.push(char_);
        }
        image.push(row);
    }
    Ok((map,image))
}

fn pad(image: &Vec<Vec<char>>, pad_char: char) -> Vec<Vec<char>> {
    // pad the given image with the given character
    let mut result = vec![vec![pad_char; image[0].len() + 4]; image.len() + 4];

    for i in 0..image.len() {
        for j in 0..image[i].len() {
            result[i + 2][j + 2] = image[i][j];
        }
    }

    return result;
}

fn lookup(image: &Vec<Vec<char>>, map: &HashMap<usize,char>, i: usize, j: usize) -> char {
    // find the 9 cells corresponding to (i,j), convert to binary,
    // and return associated char
    let mut idx = 0;
    if image[i-1][j-1] == '#' {
        idx += 2_usize.pow(8);
    }
    if image[i-1][j] == '#' {
        idx += 2_usize.pow(7);
    }
    if image[i-1][j+1] == '#' {
        idx += 2_usize.pow(6);
    }
    if image[i][j-1] == '#' {
        idx += 2_usize.pow(5);
    }
    if image[i][j] == '#' {
        idx += 2_usize.pow(4);
    }
    if image[i][j+1] == '#' {
        idx += 2_usize.pow(3);
    }
    if image[i+1][j-1] == '#' {
        idx += 2_usize.pow(2);
    }
    if image[i+1][j] == '#' {
        idx += 2_usize.pow(1);
    }
    if image[i+1][j+1] == '#' {
        idx += 2_usize.pow(0);
    }
    return map[&idx];
}

fn enhance(image: &Vec<Vec<char>>, map: &HashMap<usize,char>, pad_char: char) -> Vec<Vec<char>> {
    // enhance the image, remapping each element of the input image
    // using our indexing scheme

    // initialize result
    let mut result = vec![vec!['.'; image[0].len() + 2]; image.len() + 2];
    
    // pad the input image to account for its "infinite" size
    let padded = pad(&image, pad_char);

    // iterate through the input image, remapping each pixel
    for i in 0..result[0].len() {
        for j in 0..result.len() {
            result[i][j] = lookup(&padded, map, i + 1, j + 1);
        }
    }

    return result;
}

fn count(image: &Vec<Vec<char>>) -> u64 {
    let mut total = 0;
    for i in 0..image[0].len() {
        for j in 0..image.len() {
            if image[i][j] == '#' {
                total += 1;
            }
        }
    }
    total
}

fn print_image(image: &Vec<Vec<char>>, label: String) {
    // nice console printing for debugging
    println!("{}", label);
    for i in 0..image[0].len() {
        let mut row = String::new();
        for j in 0..image.len() {
            row.push(image[i][j]);
        }
        println!("  {}", row);
    }
}

fn main() {
    // parse input
    let (map, mut image) = parse_input(FILENAME).unwrap();
    print_image(&image, "Starting Image:".to_string());

    // enhance multiple times for Part A 
    for i in 0..50 {
        let pad_char = if i % 2 == 1 { map[&0] } else { map[&(map.len() - 1)] };
        // let pad_char = '.';
        image = enhance(&image, &map, pad_char);

        // print_image(&image, format!("Iteration #{}", (i+1)));
        // println!("Enhancing #{} with {}", i+1, pad_char);
        if i == 1 {
            println!("Part A: total lit after 2 iterations: {}", count(&image));
        }
    }
    println!("Part B: total lit after 50 iterations: {}", count(&image));
}

















