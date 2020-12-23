import csv
import numpy as np
from collections import defaultdict
from square import Square

if __name__ == "__main__":
    # load data
    data = {}
    with open("example.txt", "r") as csvfile:
        reader = csv.reader(csvfile)
        # read in all data
        idrow = next(reader)[0]
        current = int(idrow.split()[1].replace(":",""))
        for row in reader:
            # if this is an empty row, the next one is an ID
            if len(row) == 0:
                idrow = next(reader)[0]
                current = int(idrow.split()[1].replace(":",""))
            else:
                # append this row to the current data
                nprow = np.array([1 if r == "#" else 0 for r in row[0]], dtype=bool)
                if current in data:
                    data[current] = np.vstack((data[current],nprow))
                else:
                    data[current] = nprow

    # convert to our Square object
    data = {key:Square(key,value) for key,value in data.items()}

    # part 1, find the sums of each corner
    #  to solve this we note that all we need to do is find
    #  the 4 (hopefully unique) squares that have exactly
    #  2 edges matching the other arrays

    corners = []
    matches = defaultdict(list)
    # try to find the two that match
    for main_key, main_square in data.items():
        # check all other key options
        count = 0
        for key,square in data.items():
            # skip duplicate
            if key == main_key:
                continue
            # check how many edges match;
            # we need to check the normal square and its flipped orientations
            if main_square.adjacent(square) or main_square.adjacent(square.flip(0)) or main_square.adjacent(square.flip(1)):
                count += 1

        # if we've got a count of 2 that's (hopefully) a corner
        if count == 2:
            corners.append(main_key)
    
    # Print out the multiplication of the corners
    assert(len(corners)==4)
    total = 1
    for c in corners:
        total *= c
    print("Corner mult: {}".format(total))

