import csv
import copy
import math
from collections import defaultdict

if __name__ == "__main__":
    # load data
    data = defaultdict(list)
    with open("input1.txt", "r") as csvfile:
        reader = csv.reader(csvfile)
        count = 0
        last = None
        for val in next(reader):
            count += 1
            last = int(val)
            data[last].append(count)
            
    # part #1: find the 2020th number spoken
    print(last)
    goal = 30000000
    while count != goal:
        count += 1
        if len(data[last]) == 1:
            last = 0
        else:
            last = data[last][-1] - data[last][-2]
        data[last].append(count)
        print("Turn {}, {:.2f}%: {}".format(count, 100.0*count/goal, last))
    
            
