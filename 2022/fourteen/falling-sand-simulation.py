#!/usr/bin/env python3
""" Simulate the path of falling sand, presumably to escape."""

# STL

# Numpy
import numpy as np

# INPUT = "test.txt"
INPUT = "input.txt"

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

def drop_one_grain(grid, start) -> bool:
    """ Trace the path of one grain to its final location.

    Returns TRUE if the grid was modified, FALSE otherwise,
    which indicates sands are falling into the abyss.
    """
    # convenience function to return the next empty spot
    #  in the current column - if None, it's the void
    def next_below(i,j):
        assert grid[i,j] == 0, "Given a non-empty grid cell as starting point."
        while i < (grid.shape[0] - 1) and grid[i+1,j] == 0:
            i += 1
        # check if we reached the abyss
        assert grid[i,j] == 0, "Implementation bug."
        assert (i == grid.shape[0] - 1 or grid[i+1,j] > 0), "Implementation bug."
        if i == grid.shape[0] - 1:
            return None
        return i,j
    
    # get next lowest free space
    if (option := next_below(*start)) is None:
        print("Abyss")
        return False
    else:
        i,j = next_below(*start)

    # check if we can drop lower to the left or right
    if grid[i + 1, j - 1] == 0:
        return drop_one_grain(grid, (i + 1, j - 1))
    if grid[i + 1, j + 1] == 0:
        return drop_one_grain(grid, (i + 1, j + 1))

    # if we can't drop anymore, set this cell to filled
    grid[i,j] = 1
    return True

def print_grid(grid, col_start, col_end):
    """ Match visualization in problem statement. """
    def val_to_char(val):
        if val == 0:
            return "."
        elif val == 1:
            return "o"
        elif val == 255:
            return "#"
        assert False, "Implementation bug."
    for i in range(grid.shape[0]):
        string = ""
        for j in range(col_start, col_end):
            string += f" {val_to_char(grid[i,j])}"
        print(string)

if __name__ == "__main__":
    # parse input into grid
    sand = (0,500)
    grid = parse_grid(INPUT)

    # Part A - drop sand until it can't drop anymore
    count = 0
    while True:
        count += 1
        if not drop_one_grain(grid, sand):
            print("Terminal state achieved.")
            break
        print(f"iteration {count}:")
        print_grid(grid, 460, grid.shape[1])
    print(f"Final state:")
    print_grid(grid, 460, grid.shape[1])

    print(f"Part A: {grid[grid==1].size} grains fell before the Abyss took the rest.")
