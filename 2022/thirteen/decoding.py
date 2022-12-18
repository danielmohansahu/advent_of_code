#!/usr/bin/env python3
""" Decoded potentially out of order packets.

Hopefully the distress signal isn't too urgent...
"""

# STL

# INPUT = "test.txt"
INPUT = "input.txt"

def compare(left, right):
    """ Core recursive comparison operation. """
    # print(f"    {left} vs. {right}")
    if isinstance(left, int) and isinstance(right, int):
        # both elements are integers; check rules
        if left < right:
            return True
        elif left > right:
            return False
        return None
    elif isinstance(left, list) and isinstance(right, list):
        # both elements are lists; check rules
        for i in range(min(len(left), len(right))):
            if (res := compare(left[i], right[i])) is not None:
                return res
        # check if lists are equal length (i.e. we should continue)
        if len(left) == len(right):
            return None
        return len(left) < len(right)
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
        print(f"  {left} and {right} : {valid}")
        idx += 1
    print(f"Part A - found {len(valid_indices)} valid indices with sum {sum(valid_indices)}.")
        

        
