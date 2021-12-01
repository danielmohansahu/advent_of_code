import csv
import time
import copy
import numpy as np

def display(order, start):
    """ Convert to readable format.
    """
    res = []
    c = start
    while len(res) < len(order)-1:
        res.append(str(c))
        c = order[c]
    return " ".join(res)

def move(order, current):
    """ Perform a move on the given list.
    Returns the new current cup.
    """
    # get next three cups
    popped = [order[current]]
    for i in range(2):
        popped.append(order[popped[-1]])
    
    # find destination
    dest = current
    while True:
        dest = (dest-1) if (dest-1 != 0) else len(order)-1
        if dest not in popped:
            break

    # update relevant indices
    order[current] = order[popped[-1]]
    order[popped[-1]] = order[dest]
    order[dest] = popped[0]

    # get new current
    current = order[current]

    # debugging
    # print("\tcurren: {}".format(current))
    # print("\tpopped: {}".format(popped))
    # print("\tdestin: {}".format(dest))
    
    return current, order

if __name__ == "__main__":
    # load data
    string = ""
    with open("input.txt", "r") as csvfile:
        reader = csv.reader(csvfile)
        # first read in rules
        for row in reader:
            if len(row) != 0:
                string = row[0]

    # convert to a mapping from index to next value
    data = [0]*(len(string)+1)
    for i,s in enumerate(string[:-1]):
        data[int(s)] = int(string[i+1]) 
    # make sure to complete the circle of wrapping
    data[int(string[-1])] = len(data)

    # pad with up to 1000000 values
    data += [k+1 for k in range(len(data),1000000)]
    data += [int(string[0])]
    data = np.array(data,dtype=int)

    # process all moves
    iterations = 10000000
    current = int(string[0])
    ordering = copy.deepcopy(data)
    st = time.time()
    print("Iteration {}".format(0))
    for i in range(iterations):
        current,ordering = move(ordering, current)
        if ((i+1)%1000000 == 0):
            est = (time.time() - st) * (iterations/(i+1) - 1)
            print("Iteration {}, ETA: {:.2f} sec".format(i+1,est))

    # find index of 1
    idx = int(np.where(ordering == 1)[0][0])
    val1 = ordering[ordering[idx]]
    val2 = ordering[val1]
    print("Values after 1: {},{}; product: {}".format(val1,val2,val1*val2))
    



