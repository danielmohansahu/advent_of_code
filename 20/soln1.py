import sys
import csv
import numpy as np
from collections import defaultdict
from square import Square

def oriented(square, desired):
    """ Return True if the given square is oriented as desired.
    """
    success = True
    neighbors = square.neighbors
    for side,id_ in desired.items():
        if id_ is None:
            success &= (side not in neighbors)
        else:
            success &= (side in neighbors and neighbors[side] == id_)
        if not success:
            return success
    return success


def orient(squares, id_, desired):
    """ Return a copy of the square at given id_, rotated and/or flipped
         to ensure it has neighbors at the desired sides.
    """
    # check if we're given a class or id
    square = None
    if isinstance(id_, int):
        square = squares[id_]
    else:
        square = id_

    # check all permutations until aligned
    temp = square.copy()
    for i in range(3):
        # flip, if necessary
        if i == 1:
            temp = square.flip(0)
        elif i == 2:
            temp = square.flip(1)
        # rotate up to 360 degrees
        for j in range(4):
            # check stop conditions
            if oriented(temp, desired):
                return temp
            temp = temp.rotate()

    # if we've gotten this far something is wrong
    print("Failed to orient square.")
    import pdb;pdb.set_trace()

def search(picture, mask):
    """ Search through the given picture for matches with the given mask.

    Note that we only care about True elements of the mask, False can be
    anything in the picture.
    """
    key_indices = np.where(mask == 1)
    count = 0
    for i in range(picture.shape[0]-mask.shape[0]):
        for j in range(picture.shape[1]-mask.shape[1]):
            # apply mask to this subsection
            combined = np.logical_and(mask,picture[i:i+mask.shape[0],j:j+mask.shape[1]])
            # check important indices
            if np.all(combined[key_indices]):
                count += 1
    return count

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

    # now we go through and actually join the picture
    size = int(np.sqrt(len(squares)))
    picture_keys = np.zeros((size,size),dtype=int)
    oriented_squares = {}

    # start by orienting a corner to be the top left square
    #  orient and flip as needed to have no neighbors to the top or left
    start = orient(squares, corners[0], {"top": None, "left": None})
    oriented_squares[start.id] = start
    remaining_squares = list(squares.keys())
    remaining_squares.remove(start.id)
    picture_keys[0,0] = start.id

    print("Starting to build out picture.")

    # go through the rest of the picture and add each square's neighbors
    for i in range(size):
        for j in range(size):
            # skip first, already done
            if i == j == 0:
                continue
            # find this square's desired neighbors
            desired = {}
            known_sides = {}
            # handle left side
            if i == 0:
                desired["top"] = None
            else:
                desired["top"] = picture_keys[i-1,j]
                known_sides["top"] = oriented_squares[picture_keys[i-1,j]].bottom()
            # handle top side
            if j == 0:
                desired["left"] = None
            else:
                desired["left"] = picture_keys[i,j-1]
                known_sides["left"] = oriented_squares[picture_keys[i,j-1]].right()
            # handle right side (might be unknown)
            if i == size-1:
                desired["bottom"] = None
            # handle bottom side (might be unknown)
            if j == size-1:
                desired["right"] = None

            # we must have a known key here, or something's wrong
            assert(len(known_sides) != 0)

            # find the one square which has a neighbor as our known side
            target = [k for k in remaining_squares if all([squares[k].adjacent(side) for side in known_sides.values()])]

            # some sanity checks
            assert(len(target) == 1)
            target = target[0]

            # check if we need to flip this square to align
            square = squares[target]
            for side,val in known_sides.items():
                if not square.adjacent(val, strict=True):
                    # flip about axis 
                    square = square.flip(0 if side=="top" else 1)

            # orient square
            square = orient(squares, square, desired)
            oriented_squares[square.id] = square
            picture_keys[i,j] = square.id
            remaining_squares.remove(square.id)

            print("\t pixel ({},{}) done.".format(i,j))

    # combine into a real array (without borders)
    s = start.data.shape[0]-2
    picture = np.zeros((s*size,s*size),dtype=int)
    for i in range(size):
        for j in range(size):
            picture[s*i:s*(i+1),s*j:s*(j+1)] = oriented_squares[picture_keys[i,j]].data[1:-1,1:-1]

    # Actually printing the transpose, since that's what they get in the example
    picture = picture.T
    print("Combined picture: ({}x{})".format(*picture.shape))
    print(picture.T)

    # Our "sea monster" mask:
    mask = np.array([
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0],
        [1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1],
        [0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0]],
        dtype=int)

    # search our picture for monsters
    for i in range(3):
        temp = picture.copy()
        if i == 1:
            temp = np.flip(temp,0)
            print("Checking flipped picture (axis 0):")
        elif i == 2:
            temp = np.flip(temp,1)
            print("Checking flipped picture (axis 1):")
        else:
            print("Checking non-flipped picture:")

        # rotate and check
        for j in range(4):
            count = search(temp, mask)
            if count != 0:
                print(temp)
                print("Found {} sea monsters!!".format(count))
                sys.exit()
            temp = np.rot90(temp)

    import code
    code.interact(local=locals())



