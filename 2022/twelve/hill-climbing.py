#!/usr/bin/env python3
""" Trying to find cell signal (now with path planning!) """

# STL
from dataclasses import dataclass

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

@dataclass
class Node:
    dist: int
    i: int
    j: int

def djikstra(start, goal, grid):
    """ Djikstra's path search algorithm """
    # node class - contains visitation variables
    unvisited = []
    for i in range(grid.shape[0]):
        for j in range(grid.shape[1]):
            # mark start node distance as zero
            if (i,j) == start:
                unvisited.append(Node(0, i, j))
            else:
                unvisited.append(Node(np.inf, i, j))

    # convenience function to grab the next lowest cost unvisited node
    def get_next_node(nodes):
        nodes.sort(key=lambda n: n.dist)
        element = nodes.pop(0)
        return element, nodes
    current,unvisited = get_next_node(unvisited)

    # iterate until we find our goal or run out of nodes
    while len(unvisited) != 0:
        # stop condition - current is our destination node!
        if (current.i, current.j) == goal:
            print("Found goal node!")
            return current.dist
        # get potential children of current node
        for (i,j) in get_valid_children(current.i, current.j, grid):
            # find associated child node
            try:
                child = next(node for node in unvisited if (node.i == i and node.j == j))
            except StopIteration:
                # child already visited - skip
                continue
            # if unvisited, update value
            child.dist = min(child.dist, current.dist + 1)
        # select next unvisited node as the lowest cost option
        current,unvisited = get_next_node(unvisited)

    assert goal == (current.i,current.j), "Unexpectedly didn't find goal node."
    return current.dist

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
        print(f"checking {len(current)} paths...")
    # return the length of the best path
    assert len(paths) != 0, "Failed to find a successful path."
    return min([len(p) for p in paths]) - 1

if __name__ == "__main__":
    # load input grid
    start, goal, grid = load_as_grid(INPUT)
    print(f"Grid: \n{grid}")

    # find optimal path, suboptimally
    # steps = brute_force(start, goal, grid)
    steps = djikstra(start, goal, grid)
    print(f"Found shortest path in {steps}")
