import csv
import numpy as np
from itertools import product
import copy

def iterate_grid(grid):
    """ Return a copy of the grid after applying iteration rules, e.g.:

     - If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
     - If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
     - Otherwise, the seat's state does not change.
    """
    permutations = [p for p in product([-1,0,1], repeat=2)]
    permutations.pop(4) # remove (0,0)
    def surrounding(data,i,j):
        # return the values of the surrounding cells (ignoring NAN):
        result = 0
        for i_,j_ in permutations:
            # make sure this isn't an edge case
            if (0 <= (i+i_) < grid.shape[0]) and (0 <= (j+j_) < grid.shape[1]):
                val = grid[i+i_,j+j_]
                # make sure this isn't including floors
                if val != np.inf:
                    result += val
        return result

    # copy grid (next iteration)
    iteration = copy.deepcopy(grid)
    for i in range(grid.shape[0]):
        for j in range(grid.shape[1]):
            if grid[i,j] == 0 and surrounding(grid,i,j) == 0:
                # this should become occupied
                iteration[i,j] = 1
            elif grid[i,j] == 1 and surrounding(grid,i,j) >= 4:
                # this should become empty
                iteration[i,j] = 0
    return iteration

if __name__ == "__main__":
    # load data
    data = []
    with open("input1.txt", "r") as csvfile:
        reader = csv.reader(csvfile)
        for row in reader:
            if (len(row) != 0):
                data.append([r for r in row[0]])

    # convert to numpy array
    grid = []
    for row in data:
        grid.append(np.array([0 if (r == "L") else np.inf for r in row]))
    grid = np.array(grid)

    # part #1: iterate until stable
    current = grid
    new = iterate_grid(current)
    while np.any(new != current):
        current = new
        new = iterate_grid(current)

    # print out number of occupied seats (finally)
    temp = np.copy(new)
    temp[np.isinf(temp)] = 0
    print(np.sum(temp))
