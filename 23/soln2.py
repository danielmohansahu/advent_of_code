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
    temp = copy.deepcopy(order)
    temp[current] = order[popped[-1]]
    temp[dest] = popped[0]
    temp[popped[-1]] = order[dest]

    order = temp
    current = order[current]

    # debugging
    print("\tcurren: {}".format(current))
    print("\tpopped: {}".format(popped))
    print("\tdestin: {}".format(dest))
    
    return current, order

if __name__ == "__main__":
    # load data
    string = ""
    with open("example.txt", "r") as csvfile:
        reader = csv.reader(csvfile)
        # first read in rules
        for row in reader:
            if len(row) != 0:
                string = row[0]

    # convert to a mapping from index to next value
    data = np.zeros(len(string)+1,dtype=int)
    for i,s in enumerate(string[:-1]):
        data[int(s)] = int(string[i+1]) 
    # make sure to complete the circle of wrapping
    data[int(string[-1])] = int(string[0])

    # process all moves
    iterations = 10
    current = int(string[0])
    ordering = copy.deepcopy(data)
    st = time.time()
    print("Iteration {} : {}".format(0,display(ordering,current)))
    for i in range(iterations):
        current,ordering = move(ordering, current)
        if ((i+1)%1 == 0):
            est = (time.time() - st) * (iterations/(i+1) - 1)
            print("Iteration {}: {}, ETA: {:.2f} min".format(i+1,display(ordering,current),est/60))

    # find index of 1
    idx = int(np.where(ordering == 1)[0][0])
    val1 = ordering[ordering[idx]]
    val2 = ordering[val1]
    print("Values after 1: {},{}".format(val1,val2))

    import pdb;pdb.set_trace()
    



