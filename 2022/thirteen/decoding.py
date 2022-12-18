#!/usr/bin/env python3
""" Decoded potentially out of order packets.

Hopefully the distress signal isn't too urgent...
"""

# STL
import functools

# INPUT = "test.txt"
INPUT = "input.txt"

def compare(left, right):
    """ Core recursive comparison operation. """
    # print(f"    {left} vs. {right}")
    if isinstance(left, int) and isinstance(right, int):
        # both elements are integers; check rules
        if left < right:
            return -1
        elif left > right:
            return 1
        return 0
    elif isinstance(left, list) and isinstance(right, list):
        # both elements are lists; check rules
        for i in range(min(len(left), len(right))):
            if (res := compare(left[i], right[i])) != 0:
                return res
        # check if lists are equal length (i.e. we should continue)
        if len(left) == len(right):
            return 0
        if len(left) < len(right):
            return -1
        return 1
    else:
        # one item is not a list, retry
        if isinstance(left, int):
            return compare([left], right)
        return compare(left, [right])

if __name__ == "__main__":
    # parse input into a series of packet pairs
    packets = []
    with open(INPUT, "r") as inputfile:
        # get all lines
        lines = inputfile.readlines()
        # iterate through line pairs (triplets, really, including empty lines)
        lines = [l.strip() for i,l in enumerate(lines) if ((i+1) % 3 != 0)]
        # save as pairs
        for i in range(len(lines) // 2):
            packets.append((eval(lines[2*i]),eval(lines[2*i+1])))
    
    # part A - determine valid packets
    valid_indices = []
    idx = 1
    for left,right in packets:
        if (valid := compare(left,right)):
            valid_indices.append(idx)
            assert valid is not None, "Unexpected result value."
        # print(f"  {left} and {right} : {valid}")
        idx += 1
    print(f"Part A - found {len(valid_indices)} valid indices with sum {sum(valid_indices)}.")

    # for part B we need to put all the packets in order (along with some extra)
    all_packets = [[[6]],[[2]]]
    for packet in packets:
        all_packets += packet

    # sort based on given comparison
    packets = sorted(all_packets, key=functools.cmp_to_key(compare))

    # determine indices of divider packets
    decoder_1 = next(i for i,p in enumerate(packets) if p == [[2]]) + 1
    decoder_2 = next(i for i,p in enumerate(packets) if p == [[6]]) + 1
    print(f"Part B: - found decoder packets at ({decoder_1},{decoder_2}): {decoder_1 * decoder_2}")
