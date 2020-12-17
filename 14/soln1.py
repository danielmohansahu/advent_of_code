import csv
import copy
import math

def apply_mask(mask, value):
    # format value as bits
    valbits = list("{0:036b}".format(int(value)))
    
    for i,v in enumerate(mask):
        if v != "X":
            valbits[i] = v
    valbits = "".join(valbits)

    return int(valbits,2)

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
                    index = words[0].replace("mem[","").replace("]","")
                    data[index] = apply_mask(mask,words[-1])

    print(data)
    print("Memory sum: {}".format(sum(data.values())))
    
            
