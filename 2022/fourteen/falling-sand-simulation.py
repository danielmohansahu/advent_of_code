#!/usr/bin/env python3
""" Simulate the path of falling sand, presumably to escape."""

# STL

# Numpy
import numpy as np

INPUT = "test.txt"
# INPUT = "input.txt"

def parse_grid(filename):
    """ Get grid representation from input file."""
    rocks = []
    rows = 0
    cols = 500 + 1
    with open(filename, "r") as inputfile:
        # represent as a list of indices indicating straight lines
        for line in inputfile.readlines():
            indices = []
            for pair in line.strip().split("->"):
                i,j = pair.strip().split(",")
                i = int(i)
                j = int(j)
                indices.append(np.array([i,j], dtype=int))
                # save max indices
                cols = max(cols, i + 1)
                rows = max(rows, j + 1)
            rocks.append(np.array(indices, dtype=int))

    # construct grid from rocks
    # 1) get size of grid
    # 2) construct empty grid
    # 3) populate with ROCKS (255) - sand is (1), free is (0)
    grid = np.zeros((rows, cols))
    for rock in rocks:
        # iterate through rocks, drawing as we go
        for r in range(len(rock) - 1):
            # note - we're assuming each rock is 1D!
            diff = rock[r + 1] - rock[r]
            dist = abs(sum(diff)) + 1
            dir_ = diff / dist
            for d in range(dist):
                i,j = rock[r] + d * dir_
                grid[int(j),int(i)] = 255

    # debugging
    if filename == "test.txt":
        print(grid[:,494:504])
    return grid

if __name__ == "__main__":
    sand = np.array([0,500])
    grid = parse_grid(INPUT)
