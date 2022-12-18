#!/usr/bin/env python3
""" Trying to find cell signal (now with path planning!) """

# STL

# Numpy
import numpy as np

# Inputs
# INPUT = "test.txt"
INPUT = "input.txt"

def load_as_grid(filename : str):
    """ Load the input, convert to integers, and return start / goal indices. """
    grid = None
    with open(filename, "r") as gridfile:
        for i,line in enumerate(gridfile.readlines()):
            # strip whitespace
            line = line.strip()
            # check for start / end characters; if found, log their location
            if (j := line.find("S")) != -1:
                line = line.replace("S","a")
                start = (i,j)
            if (j := line.find("E")) != -1:
                line = line.replace("E","z")
                goal = (i,j)
            # save this row
            row = [ord(c) - ord("a") for c in line]
            if grid is None:
                grid = row
            else:
                grid = np.vstack([grid,row])
    assert start, "Start not found!"
    assert goal , "Goal not found!"
    return (start,goal,grid)

def get_valid_children(i, j, grid):
    """ Return all valid children next to the given node. """
    val = grid[i,j]
    children = []
    if (i > 0) and (grid[i - 1, j] - val <= 1):
        children.append((i - 1, j))
    if (i + 1 < grid.shape[0]) and (grid[i + 1, j] - val <= 1):
        children.append((i + 1, j))
    if (j > 0) and (grid[i, j - 1] - val <= 1):
        children.append((i, j - 1))
    if (j + 1 < grid.shape[1]) and (grid[i, j + 1] - val <= 1):
        children.append((i, j + 1))
    return children

def brute_force(start, goal, grid):
    """ Quasi-Djikstra, based on my faulty memory. """
    # initialize loop variables
    paths = []          # 
    current = [[start]]
    while len(current) != 0:
        # initialize the next set of nodes to check
        next_loop_paths = []
        # iterate through current nodes, checking for viable moves
        for path in current:
            for child in get_valid_children(*path[-1], grid):
                newpath = path + [child]
                # check for success:
                if child == goal:
                    paths.append(newpath)
                    continue
                # otherwise , check if this path should continue to the next round
                if child not in path:
                    next_loop_paths.append(newpath)
        # set our next loop paths for, well, the next loop
        current = next_loop_paths
    # return the length of the best path
    assert len(paths) != 0, "Failed to find a successful path."
    return min([len(p) for p in paths]) - 1

if __name__ == "__main__":
    # load input grid
    start, goal, grid = load_as_grid(INPUT)
    print(f"Grid: \n{grid}")

    # find optimal path, suboptimally
    steps = brute_force(start, goal, grid)
    print(f"Found shortest path in {steps}")
