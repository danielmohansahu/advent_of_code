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
    squares = {key:Square(key,value) for key,value in data.items()}
    size = int(np.sqrt(len(squares)))
    picture = np.zeros((size,size))

    # part 1, find the product of each corner

    # first, we go through and find which squares are adjacent to each other
    for main_key,main_square in squares.items():
        # get edges
        top = set([main_square.top(), main_square.top()[::-1]])
        bottom = set([main_square.bottom(), main_square.bottom()[::-1]])
        left = set([main_square.left(), main_square.left()[::-1]])
        right = set([main_square.right(), main_square.right()[::-1]])
        # evaluate this square against every other square
        for key,square in squares.items():
            edges = square.edges()
            # skip ourselves
            if key == main_key:
                continue
            # check if our top has a neighbor 
            if len(top.intersection(edges)) != 0:
                main_square.set_neighbor("top", key)
            elif len(bottom.intersection(edges)) != 0:
                main_square.set_neighbor("bottom", key)
            elif len(left.intersection(edges)) != 0:
                main_square.set_neighbor("left", key)
            elif len(right.intersection(edges)) != 0:
                main_square.set_neighbor("right", key)

    # find our corners (those that only have 2 neighbors)
    corners = [k for k,s in squares.items() if s.is_corner()]
    assert(len(corners)==4)

    prod = 1
    for c in corners:
        prod *= c 
    print("Corner product: {}".format(prod))



