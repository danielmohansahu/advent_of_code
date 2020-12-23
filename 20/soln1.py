import csv
import numpy as np

def edge_options(square):
    """Returns all permutations of edges (4 edges + reversed)
    """
    shape = square.shape[0]
    opts = [
        square[0,:shape],
        square[shape-1,:shape],
        square[:shape,0],
        square[:shape,shape-1],
        np.flip(square[0,:shape]),
        np.flip(square[shape-1,:shape]),
        np.flip(square[:shape,0]),
        np.flip(square[:shape,shape-1])
    ]
    # convert to something a little easier to compare
    #  could be a lot more efficient, but eh
    result = []
    for opt in opts:
        result.append("".join(["1" if v else "0" for v in opt]))
    return set(result)

if __name__ == "__main__":
    # load data
    data = {}
    with open("input.txt", "r") as csvfile:
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
    print(data.keys())

    # part 1, find the sums of each corner
    #  to solve this we note that all we need to do is find
    #  the 4 (hopefully unique) squares that have exactly
    #  2 edges matching the other arrays
    all_edges = {id_:edge_options(s) for id_,s in data.items()}

    corners = []
    # try to find the two that match
    for main_key, main_edges in all_edges.items():
        # check all other key options
        count = 0
        for key,edges in all_edges.items():
            if key == main_key:
                continue
            # check how many edges match;
            overlap = set.intersection(main_edges,edges)
            if len(overlap) > 0:
                # check for an unexpected edge case:
                print(overlap)
                # assert(len(overlap) == 1)
                # another match
                count += 1
        # if we've got a count of 2 that's (hopefully) a corner
        if count == 2:
            corners.append(main_key)
    
    # Print out the multiplication of the corners
    assert(len(corners)==4)
    total = 1
    for c in corners:
        total *= c
    print("Corner mult: {}".format(total))

        



