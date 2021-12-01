import csv
import copy
import math

if __name__ == "__main__":
    # load data
    data = []
    with open("input1.txt", "r") as csvfile:
        reader = csv.reader(csvfile)
        # skip timestamp; unused
        next(reader)
        times = next(reader)

    # other examples
    # times = [17,"x",13,19]
    # times = [67,7,59,61]
    # times = [67,"x",7,59,61]
    # times = [67,7,"x",59,61]
    # times = [1789,37,47,1889]

    print(times)

    # part #2, iterate through times (at increments of times[0])

    # pre-process times and minutes
    pure_times = []
    pure_minutes = []
    minute = 0
    for t in times:
        if t != "x":
            pure_times.append(int(t))
            pure_minutes.append(minute)
        minute += 1

    # offset by the first number of minutes
    # offset = pure_minutes[pure_times.index(max(pure_times))]
    # pure_minutes = [m - offset for m in pure_minutes]

    # sort from highest to lowest
    pure = zip(pure_times,pure_minutes)
    # pure.sort()
    # pure.reverse()
    print(pure)
    # print(offset)

    # initialize loop variables
    print_count = 0
    timestamp = 100000000000000
    success = False
    increment = pure_times[0]
    while not success:
        print_count += 1
        if print_count % 1000000 == 0:
            print(timestamp)

        # check stop conditions
        success = True
        for t,m in pure:
            if (timestamp + m) % t != 0:
                success = False
                break

        # if we've gotten this far this is the right timestamp
        if not success:
            timestamp += increment

    # include offset
    # timestamp -= offset
    print("Success at time {}".format(timestamp))
