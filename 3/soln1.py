import csv
import numpy as np

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

    # calculate traversal costs with the given primitive (down 1, right 3)
    column = 0 
    cost = data[0,column]
    for row in range(1, data.shape[0]):
        # get next column
        column = (column + 3) % data.shape[1]

        # calculate cost (num trees)
        cost += data[row,column]

    print(cost)

