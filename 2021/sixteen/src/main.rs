// Decode the BITS Transmission
// https://adventofcode.com/2021/day/16

use std::vec::Vec;
use std::collections::HashMap;

// our inputs to parse

// examples for Part A
// const INPUT: &str = "D2FE28";
// const INPUT: &str = "8A004A801A8002F478";
// const INPUT: &str = "620080001611562C8802118E34";
// const INPUT: &str = "C0015000016115A2E0802F182340";
// const INPUT: &str = "A0016C880162017C3686B18A3D4780";
// const INPUT: &str = "A0016C880162017C3686B18A3D4780";

// examples for Part B
// const INPUT: &str = "C200B40A82";
// const INPUT: &str = "04005AC33890";
// const INPUT: &str = "880086C3E88112";
// const INPUT: &str = "CE00C43D881120";
// const INPUT: &str = "D8005AC2A8F0";
// const INPUT: &str = "F600BC2D8F";
// const INPUT: &str = "9C005AC2F8F0";
// const INPUT: &str = "9C0141080250320F1802104A08";

// the real input
const INPUT: &str = "E058F79802FA00A4C1C496E5C738D860094BDF5F3ED004277DD87BB36C8EA800BDC3891D4AFA212012B64FE21801AB80021712E3CC771006A3E47B8811E4C01900043A1D41686E200DC4B8DB06C001098411C22B30085B2D6B743A6277CF719B28C9EA11AEABB6D200C9E6C6F801F493C7FE13278FFC26467C869BC802839E489C19934D935C984B88460085002F931F7D978740668A8C0139279C00D40401E8D1082318002111CE0F460500BE462F3350CD20AF339A7BB4599DA7B755B9E6B6007D25E87F3D2977543F00016A2DCB029009193D6842A754015CCAF652D6609D2F1EE27B28200C0A4B1DFCC9AC0109F82C4FC17880485E00D4C0010F8D110E118803F0DA1845A932B82E200D41E94AD7977699FED38C0169DD53B986BEE7E00A49A2CE554A73D5A6ED2F64B4804419508B00584019877142180803715224C613009E795E58FA45EA7C04C012D004E7E3FE64C27E3FE64C24FA5D331CFB024E0064DEEB49D0CC401A2004363AC6C8344008641B8351B08010882917E3D1801D2C7CA0124AE32DD3DDE86CF52BBFAAC2420099AC01496269FD65FA583A5A9ECD781A20094CE10A73F5F4EB450200D326D270021A9F8A349F7F897E85A4020CF802F238AEAA8D22D1397BF27A97FD220898600C4926CBAFCD1180087738FD353ECB7FDE94A6FBCAA0C3794875708032D8A1A0084AE378B994AE378B9A8007CD370A6F36C17C9BFCAEF18A73B2028C0A004CBC7D695773FAF1006E52539D2CFD800D24B577E1398C259802D3D23AB00540010A8611260D0002130D23645D3004A6791F22D802931FA4E46B31FA4E4686004A8014805AE0801AC050C38010600580109EC03CC200DD40031F100B166005200898A00690061860072801CE007B001573B5493004248EA553E462EC401A64EE2F6C7E23740094C952AFF031401A95A7192475CACF5E3F988E29627600E724DBA14CBE710C2C4E72302C91D12B0063F2BBFFC6A586A763B89C4DC9A0";

fn hex2bits(hex: &str) -> Vec<u8> {
    // initialize result
    let mut bits: Vec<u8> = Vec::new();

    // set up hashmap of hex->bit
    let mut hex2bit: HashMap<char,Vec<u8>> = HashMap::new();
    hex2bit.insert('0', vec![0,0,0,0]);
    hex2bit.insert('1', vec![0,0,0,1]);
    hex2bit.insert('2', vec![0,0,1,0]);
    hex2bit.insert('3', vec![0,0,1,1]);
    hex2bit.insert('4', vec![0,1,0,0]);
    hex2bit.insert('5', vec![0,1,0,1]);
    hex2bit.insert('6', vec![0,1,1,0]);
    hex2bit.insert('7', vec![0,1,1,1]);
    hex2bit.insert('8', vec![1,0,0,0]);
    hex2bit.insert('9', vec![1,0,0,1]);
    hex2bit.insert('A', vec![1,0,1,0]);
    hex2bit.insert('B', vec![1,0,1,1]);
    hex2bit.insert('C', vec![1,1,0,0]);
    hex2bit.insert('D', vec![1,1,0,1]);
    hex2bit.insert('E', vec![1,1,1,0]);
    hex2bit.insert('F', vec![1,1,1,1]);

    // iterate through input string, converting chars as we go
    for char_ in hex.to_string().chars() {
        let mut binary = hex2bit[&char_].clone();
        bits.append(&mut binary);
    }

    return bits;
}

fn binary2dec(bits: &[u8]) -> u64 {
    let mut dec: u64 = 0;
    for (i,b) in bits.iter().enumerate() {
        dec += (*b as u64) * 2_u64.pow((bits.len() - i - 1) as u32);
    }
    // println!("{:?} -> {}", bits, dec);
    return dec;
}

fn decode(bits: &[u8], count: &mut u64) -> (u64,usize) {
    // recursively decode the given bit array
    //  we assume this is a single packet

    // check for break conditions
    if bits.len() < 6 {
        println!("Found end of packet.");
        return (0,bits.len());
    }

    // get packet version
    let version = binary2dec(&bits[0..3]);
    let type_ = binary2dec(&bits[3..6]);

    // add version to our count
    *count += version;

    // initialize packet length (result)
    let mut packet_length: usize = 0;
    let mut value: u64 = 0;

    // check packet type
    if type_ == 4 {
        println!("\t decoding literal version {}.", version);
        
        // initialize subvector
        let mut literal_bits: Vec<u8> = Vec::new();
        let mut idx = 6;
        loop {
            // add the last 4 bits to our subvector
            let last = bits[idx] == 0;
            for i in (idx+1)..(idx+5) {
                literal_bits.push(bits[i]);
            }
            // increment out index
            idx += 5;

            // stop condition
            if last {
                break;
            }
        }

        // convert to decimal
        let dec = binary2dec(&literal_bits);
        println!("\t\t found literal {}", dec);

        // update our start index to point to the next packet
        value = dec;
        packet_length = idx;
    } else {
        println!("\t decoding operator type {} version {}.", type_, version);
        
        // check what type of operator packet this is
        let op_type = bits[6];
        let mut values: Vec<u64> = Vec::new();
        match op_type {
            0 => {
                // the next 15 bits give the total length of packets
                let length = binary2dec(&bits[7..22]);
                println!("\t\t next packets have length {}", length);

                // decode these packets until we finish the length
                let end: usize = 22 + length as usize;
                let mut cur: usize = 22;
                loop {
                    let (val,len) = decode(&bits[cur..end], count);
                    values.push(val);
                    cur += len;
                    if cur == end {
                        break;
                    }
                }

                // update out estimate of packet length and result
                packet_length = cur;
            },
            1 => {
                // the next 11 bits give the total number of packets
                let size = binary2dec(&bits[7..18]);
                println!("\t\t next {} packets to be parsed.", size);

                let mut cur = 18;
                for i in 0..size {
                    let (val,len) = decode(&bits[cur..], count);
                    values.push(val);
                    cur += len;
                }

                // update our estimate of packet length
                packet_length = cur;
            },
            _ => panic!("Unexpected value for operator type: {}", op_type)
        }

        // not apply expected operation to our values to get the resulting value
        value = match type_ {
            0 => values.iter().sum(),
            1 => values.iter().product(),
            2 => *values.iter().min().unwrap(),
            3 => *values.iter().max().unwrap(),
            4 => value,     // null operation - it's a string literal
            5 => {
                assert_eq!(values.len(), 2);
                if values[0] > values[1] {1} else {0}
            },
            6 => {
                assert_eq!(values.len(), 2);
                if values[0] < values[1] {1} else {0}
            },
            7 => {
                assert_eq!(values.len(), 2);
                if values[0] == values[1] {1} else {0}
            },
            _ => panic!("Unexpected operator type!")
        };
    }

    // return the length of the packet
    return (value, packet_length);
}



fn main() {
    // first, translate to binary:
    let binary = hex2bits(&INPUT);
    // println!("Hex to Binary: \n{:?}\n{:?}", INPUT, binary);
    
    // solve for part A
    let mut count: u64 = 0;
    let (val,_) = decode(&binary, &mut count);
    println!("Part A got version count: {}", count);
    println!("Part B got a value of: {}", val);

}


