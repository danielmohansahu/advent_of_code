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
    if (i > 0) and (val - grid[i - 1, j] <= 1):
        children.append((i - 1, j))
    if (i + 1 < grid.shape[0]) and (val - grid[i + 1, j] <= 1):
        children.append((i + 1, j))
    if (j > 0) and (val - grid[i, j - 1] <= 1):
        children.append((i, j - 1))
    if (j + 1 < grid.shape[1]) and (val - grid[i, j + 1] <= 1):
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
    visited = []
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

    # iterate to populate entire grid's distances
    goal_dist = np.inf
    while len(unvisited) != 0:
        # if this is our goal, save its distane but don't stop iterating
        if (current.i, current.j) == goal:
            goal_dist = current.dist
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
        visited.append(current)
        current,unvisited = get_next_node(unvisited)
    visited.append(current)
    assert len(visited) == grid.size, "Didn't flesh out the whole map!"
    return goal_dist, visited

if __name__ == "__main__":
    # load input grid
    start, goal, grid = load_as_grid(INPUT)

    # Part A find optimal path, suboptimally
    steps, nodes = djikstra(goal, start, grid)
    print(f"Part A: Found shortest path in {steps}")

    # Part B - find shortest path from any low lying grid cell
    assert len(nodes) == grid.size, "Not enough nodes found for Part B!"

    # find all cells with values of 0 (== 'a')
    candidates = []
    for i in range(grid.shape[0]):
        for j in range(grid.shape[1]):
            if grid[i,j] == 0:
                candidates.append((i,j))
    
    # use the solved path from Part A to get the distance from goal to start for each candidate
    shortest_dist = np.inf
    shortest = None
    for i,candidate in enumerate(candidates):
        try:
            node = next(node for node in nodes if (node.i,node.j) == candidate)
        except StopIteration:
            import pdb;pdb.set_trace()
        if node.dist < shortest_dist:
            print(f" found a better path {candidate}")
            shortest_dist = node.dist
            shortest = candidate
    print(f"Part B: Best path starts at {shortest} with {shortest_dist} steps required.")

