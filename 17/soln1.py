import csv
import numpy as np

def iterate(state):
    pad = 1

    # construct a temporary (padded) array for analysis
    padded = np.pad(state, pad, mode="constant")
    shape = padded.shape
    result = np.copy(padded)

    # iterate through the array, considering the number of active neighbors
    for i in range(shape[0]):
        for j in range(shape[1]):
            for k in range(shape[2]):
                # get number of positive neighbors
                xs = (max(i-1,0),min(i+2,shape[0]))
                ys = (max(j-1,0),min(j+2,shape[1]))
                zs = (max(k-1,0),min(k+2,shape[2]))
                neighbors = padded[xs[0]:xs[1],ys[0]:ys[1],zs[0]:zs[1]]

                # count of positive neighbors is count MINUS our value
                count = neighbors.sum() - (1 if padded[i,j,k] else 0)

                # apply our rules
                if padded[i,j,k] and (count not in (2,3)):
                    result[i,j,k] = False
                elif not padded[i,j,k] and (count == 3):
                    result[i,j,k] = True
    
    # @TODO trim unused zero dimensions?
    return result

if __name__ == "__main__":
    # load data
    array = None
    with open("input.txt", "r") as csvfile:
        reader = csv.reader(csvfile)
        # first read in rules
        for row in reader:
            if len(row) != 0:
                # convert to boolean array
                line = np.array([(r == "#") for r in row[0]],bool)
                if array is None:
                    array = line
                else:
                    array = np.vstack((array,line))

    # add the Z axis
    array.resize(*array.shape,1)

    # part 1, iterate 6 times
    result = np.copy(array)
    for i in range(6):
        result = iterate(result)
        print("After iterating {} times: \n{}".format(i+1,result))

    print("{} active cubes.".format(result.sum()))

