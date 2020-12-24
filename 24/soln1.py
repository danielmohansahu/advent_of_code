"""
Hexagon grid reference: https://www.redblobgames.com/grids/hexagons/
"""

import csv
import copy
import numpy as np
import time

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

def borders_flipped(array):
    """ Return true if any of the border elements are flipped.
    """
    return  array[0,:,:].any() or array[-1,:,:].any() or array[:,0,:].any() or array[:,-1,:].any() or array[:,:,0].any() or array[:,:,-1].any()

def process_day(state, origin):
    """ Apply the tile flipping rules to the floor for a single day.
    """
    # create a padded array (simplifies handling of edge cases)
    current = np.pad(state, 1)
    origin += np.ones(3,dtype=int)

    # initialize neighbor permutations
    neighbor_idx = np.array([
        [0,1,-1],
        [0,-1,1],
        [1,0,-1],
        [-1,0,1],
        [-1,1,0],
        [1,-1,0]
    ])

    # if we have flipped elements on the boundary we'll permanantly pad (another layer)
    if borders_flipped(state):
        print("Padding results.")
        current = np.pad(current, 1)
        origin += np.ones(3,dtype=int)

    # then go through each index and check the rules
    result = current.copy()
    processed = np.zeros(result.shape,dtype=bool)

    # consider all the flipped tiles first
    hexes = np.array(np.where(current)).T.tolist()

    # we should only need to process twice; one for all the black tiles
    #  and one for their white neighbors
    for i in range(2):
        # initialize next round's output
        new_hexes = []
        for hex_ in hexes:
            # get the neighbors
            neighbors = hex_ + neighbor_idx

            # get the state of each of the neighbors
            indices = tuple(neighbors.T)
            flipped = current[indices].sum()
            checked = processed[indices]
            new_hexes += neighbors[np.logical_not(checked)].tolist()

            # our rules are:
            #  If Flipped: change to white if count != 1,2
            #  If not Flipped: change to black if count == 2
            idx = tuple(hex_)
            cur = current[idx]
            if cur and flipped not in [1,2]:
                result[idx] = False
            elif not cur and flipped == 2:
                result[idx] = True

            # mark as processed
            processed[idx] = True

        # update our processing list
        hexes = new_hexes

    # while possible remove padding
    while not borders_flipped(result):
        result = result[1:-1,1:-1,1:-1]
        origin -= np.ones(3,dtype=int)
    return result, origin

if __name__ == "__main__":
    # load data
    moves = []
    with open("input.txt", "r") as csvfile:
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

    for i in range(100):
        state, origin = process_day(state, origin)
        print("Day {}: {}".format(i+1,np.sum(state)))
    

