import csv
import numpy as np

def traverse(data, row_step, col_step):
    # calculate the traversal cost for the given data set
    # with the given step sizes

    # prevent infinite looping (I know, premature)
    max_iterations = 2 * data.shape[0]

    # initialize search vars
    column = 0
    row = 0
    count = 0
    cost = data[0,0]

    # traverse until we've reached the end
    while count != max_iterations:
        count += 1
        row += row_step
        column = (column + col_step) % data.shape[1]

        # check stop conditions
        if row >= data.shape[0]:
            break

        # otherwise, calculate cost
        cost += data[row,column]

    return cost

if __name__ == "__main__":
    # load data
    data = []
    with open("input1.txt", "r") as csvfile:
       reader = csv.reader(csvfile)
       for row in reader:
           if len(row) != 0:
               temp = row[0].replace(".", '0').replace("#" , '1')
               data.append([int(t) for t in temp])

    # convert to numpy format (easier to index)
    data = np.array(data)

    # calculate the cost for each of the following steps
    cost1 = traverse(data, 1, 1)
    cost2 = traverse(data, 1, 3)
    cost3 = traverse(data, 1, 5)
    cost4 = traverse(data, 1, 7)
    cost5 = traverse(data, 2, 1)

    print(cost1 * cost2 * cost3 * cost4 * cost5)

