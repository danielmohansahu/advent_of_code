import csv
import copy
from itertools import cycle,islice

def move(order, current):
    """ Perform a move on the given list.
    Returns the new current cup.
    """
    # some handy objects
    size = len(order)
    cyclical = cycle(order)

    # get index of current position
    cur_idx = order.index(str(current))
    
    # initialize output as the ordering without removed cups
    popped = "".join(islice(cycle(order),cur_idx+1,cur_idx+4))
    result = copy.deepcopy(order)
    for p in popped:
        result = result.replace(p,"")

    # find destination cup
    dest = current
    success = False
    while not success:
        # decrement, and check if this wraps around
        dest -= 1
        if dest == 0:
            dest = 9
        if result.count(str(dest)) == 1:
            success = True

    # get destination index
    dest_idx = result.index(str(dest))

    # insert popped section
    result = result[:dest_idx+1] + popped + result[dest_idx+1:]

    # get new current cup and return
    cur_idx = result.index(str(current))
    new_current = int("".join(islice(cycle(result), cur_idx+1, cur_idx+2)))

    # debugging
    # print("\tcurren: {}".format(current))
    # print("\tpopped: {}".format(popped))
    # print("\tdestin: {}".format(dest))

    return new_current, result

if __name__ == "__main__":
    # load data
    data = ""
    with open("input.txt", "r") as csvfile:
        reader = csv.reader(csvfile)
        # first read in rules
        for row in reader:
            if len(row) != 0:
                data = row[0]

    # process 100 moves
    current = int(data[0])
    ordering = copy.deepcopy(data)
    print("Iteration {}: {}".format(0,ordering))
    for i in range(100):
        current,ordering = move(ordering, current)
        print("Iteration {}: {}".format(i+1,ordering))

    # reorder around 1 for output
    one_idx = ordering.index('1')
    result = ordering[one_idx+1:] + ordering[:one_idx]
    print("Final ordering: {}".format(result))



