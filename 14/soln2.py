import csv
import copy
import math
import itertools

def apply_mask(mask, value):
    # get all possible memory permutations
    valbits = list("{0:036b}".format(int(value)))

    # first override
    for i,v in enumerate(mask):
        if v != "0":
            valbits[i] = v
    # print(valbits)

    # try a brute force permutation
    results = [""]
    for v in valbits:
        if v == "X":
            # apply 0 to the first list and 1 to the other
            r1 = [(r + "0") for r in results]
            r2 = [(r + "1") for r in results]
            results = r1 + r2
        else:
            # just append to all elements
            results = [r + v for r in results]
        # print(results)
    
    # convert results to int:
    perms = []
    for result in results:
        perms.append(int(result,2))
    # print(len(perms))

    return perms

if __name__ == "__main__":
    # load data
    data = {}
    mask = None
    with open("input1.txt", "r") as csvfile:
        reader = csv.reader(csvfile)
        for row in reader:
            if len(row) != 0:
                words = row[0].split()
                if words[0] == "mask":
                    mask = words[-1]
                else:
                    indices = apply_mask(mask, words[0].replace("mem[","").replace("]",""))
                    for index in indices:
                        data[index] = int(words[-1])

    print("Memory sum: {}".format(sum(data.values())))
    
            
