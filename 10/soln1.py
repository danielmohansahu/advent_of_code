import csv
import numpy as np
import copy

def arrangements(n, max_jump=3):
    """ Calculate the number of permutations of N
    contiguous numbers, where the max jump is the maximum
    difference for a valid permutation.
    """
    current = [0]
    perms = 0
    end = n-1
    while len(current) != 0:
        new = []
        for c in current:
            if c == end:
                perms += 1
                continue
            for i in range(1,max_jump+1):
                if c + i > n:
                    break
                new.append(c + i)
        current = new
    return perms

if __name__ == "__main__":
    # load data
    data = []
    with open("input1.txt", "r") as csvfile:
        reader = csv.reader(csvfile)
        for row in reader:
            if (len(row) != 0):
                data.append(int(row[0]))

    # add the outlet and your device
    data.append(0)
    data.sort()
    data.append(data[-1]+3)

    # part #1: find the diff
    data_array = np.array(data)
    diff = np.diff(data_array).tolist()
    print(diff.count(1) * diff.count(3))

    # part #2: find all possible permutations 
    # of valid adapter arrangements
    perms = 1
    subsection_length = 0
    for d in diff:
        # break into subsections with diffs of i
        if d == 1:
            subsection_length += 1
        else:
            # calculate the previous subsections permutations
            perm = arrangements(subsection_length + 1)
            subsection_length = 0
            perms *= perm
    print(perms)
