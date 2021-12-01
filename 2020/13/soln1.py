import csv
import copy
import math

if __name__ == "__main__":
    # load data
    data = []
    with open("input1.txt", "r") as csvfile:
        reader = csv.reader(csvfile)
        timestamp = int(next(reader)[0])
        times = [int(v) for v in next(reader) if v != "x"]

    # part #1, get nearest departure
    next_times = []
    min_time = 0
    min_ = 1000
    for t in times:
        # edge case; this will leave at precisely the right time
        if timestamp % t == 0:
            next_times.append(0)
        else:
            rem = math.floor(float(timestamp)/t)
            next_times.append(t * (rem+1) - timestamp)

        # check if this was the lowest value
        if next_times[-1] < min_:
            min_ = next_times[-1]
            min_time = t
            print("New min: ({},{})".format(min_,min_time))
    print(min_ * min_time)
