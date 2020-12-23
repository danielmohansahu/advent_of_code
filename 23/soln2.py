import csv
import time
import copy
import numpy as np
from itertools import cycle,islice
from numba import jit

@jit(nopython=True)
def first_index(array, item, hint=0):
    # first start at the hint
    for idx, val in np.ndenumerate(array[hint:]):
        if val == item:
            return idx[0]
    # If no item was found start again at the beginning
    for idx, val in np.ndenumerate(array):
        if val == item:
            return idx[0]

@jit(nopython=True)
def first_index_rev(array, item, hint=-1):
    # first start at the hint
    for idx, val in np.ndenumerate(array[hint::-1]):
        if val == item:
            return idx[0]
    # If no item was found start again at the beginning
    for idx, val in np.ndenumerate(array[-1::-1]):
        if val == item:
            return idx[0]

def move(order, current, max_):
    """ Perform a move on the given list.
    Returns the new current cup.
    """

    # get index of current position
    cur_idx = int(first_index(order, current))
    
    # initialize output as the ordering with removed cups
    popped = np.array(list(islice(cycle(order),cur_idx+1,cur_idx+4)))

    # some optimization
    result = np.copy(order)
    if np.all(order[cur_idx+1:cur_idx+4] == popped):
        result[cur_idx+1:-3] = order[cur_idx+4:]
    else:
        result[:-3] = order[~np.in1d(order,popped)]

    # find destination cup
    dest = current
    success = False
    while not success:
        # decrement, and check if this wraps around
        dest -= 1
        if dest == 0:
            dest = max_
        # check if it's in our popped list  
        if dest not in popped:
            success = True

    # get destination index
    dest_idx = int(first_index_rev(result, dest, dest))

    # insert popped section
    result[dest_idx+4:] = result[dest_idx+1:-3]
    result[dest_idx+1:dest_idx+4] = popped

    # get new current cup and return
    cur_idx = cur_idx if (result[cur_idx] == current) else int(first_index(result, current, cur_idx))
    new_current = list(islice(cycle(result), cur_idx+1, cur_idx+2))[0]

    # debugging
    # print("\tcurren: {}".format(current))
    # print("\tpopped: {}".format(popped))
    # print("\tdestin: {}".format(dest))
    
    return new_current, result

if __name__ == "__main__":
    # load data
    data = ""
    with open("example.txt", "r") as csvfile:
        reader = csv.reader(csvfile)
        # first read in rules
        for row in reader:
            if len(row) != 0:
                data = row[0]

    # pad with the remaining 1000000
    data = [int(d) for d in data]
    data = np.array(data + list(range(max(data)+1,1000000+1)))

    # process 10000000 moves
    iterations = 10000000
    current = data[0]
    max_ = np.max(data)
    ordering = copy.deepcopy(data)
    print("Iteration {}".format(0))
    st = time.time()
    for i in range(iterations):
        current,ordering = move(ordering, current, max_)
        if (i%1000 == 0 and i!=0):
            est = (time.time() - st) * (iterations/(i+1) - 1)
            print("Iteration {}, ETA: {:.2f} min".format(i+1,est/60))

    # find index of 1
    idx = int(np.where(ordering == 1)[0][0])
    after_one = list(islice(cycle(ordering), idx, idx+3))

    import pdb;pdb.set_trace()
    



