"""
Hexagon grid reference: https://www.redblobgames.com/grids/hexagons/
"""

import csv
import copy
import numpy as np

def to_xyz(move):
    """ Convert a given sequence of moves (from 0,0,0) to Cubic Coordinates.
    """
    string = copy.deepcopy(move)
    coords = np.zeros((1,3),dtype=np.int)
    while len(string) != 0:
        if string[0] == "e":
            coords += np.array([1,-1,0])
            string = string[1:]
        elif string[0] == "w":
            coords += np.array([-1,1,0])
            string = string[1:]
        elif string[:2] == "se":
            coords += np.array([0,-1,1])
            string = string[2:]
        elif string[:2] == "sw":
            coords += np.array([-1,0,1])
            string = string[2:]
        elif string[:2] == "ne":
            coords += np.array([1,0,-1])
            string = string[2:]
        elif string[:2] == "nw":
            coords += np.array([0,1,-1])
            string = string[2:]
        else:
            print("Invalid coordinate")
            import pdb;pdb.set_trace()
    return coords

def process_day(state):
    """ Apply the tile flipping rules to the floor for a single day.
    """
    return state

if __name__ == "__main__":
    # load data
    moves = []
    with open("example.txt", "r") as csvfile:
        reader = csv.reader(csvfile)
        # first read in moves
        for row in reader:
            if len(row) != 0:
                moves.append(row[0])

    # convert to coordinate frame representation
    tiles = np.array([to_xyz(m) for m in moves])

    # create an appropriately sized and shifted array
    min_ = tiles[:,0].min(0)
    max_ = tiles[:,0].max(0)
    state = np.zeros(max_ - min_ + np.array([1,1,1]),dtype=bool)
    origin = -min_

    # apply our known tiles
    for tile in tiles:
        idx = tuple(origin + tile[0])
        state[idx] = not state[idx]

    # count the number of tiles flipped an odd number of times
    print("{} tiles flipped".format(np.sum(state)))

    # part #2: evaluate the state day by day

    # first we convert to a more useful format
    

