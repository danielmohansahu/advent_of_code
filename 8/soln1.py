import csv
import copy

def execute(data):
    """ Execute boot instructions until stop condition is met.
    """
    # process the instructions until we hit a duplicate
    acc = 0
    i = 0
    executed = set()
    while i not in executed:
        # add this instruction to the list
        executed.add(i)

        # process it
        instruction, val = data[i]
        if instruction == "nop":
            # null operation, pass
            i += 1
        elif instruction == "acc":
            # accumulate value
            acc += int(val)
            i += 1
        elif instruction == "jmp":
            # jump to new line
            i += int(val)

        # check for success (non-infinite loop)
        if i == len(data):
            return (True, acc)
    return (False, acc)

if __name__ == "__main__":
    # load data
    data = []
    with open("input1.txt", "r") as csvfile:
        reader = csv.reader(csvfile, delimiter='\n')
        for row in reader:
            if (len(row) != 0):
                data.append(row[0].split())

    # part #1: process until repeat instruction
    _,acc = execute(data)
    print("Accumulator before repeat: {}".format(acc))

    # part #2: nop vs. jmp
    for i, (instruction) in enumerate(data):
        if instruction[0] != "acc":
            # try switching this to the other instruction
            data_copy = copy.deepcopy(data)
            data_copy[i][0] = "jmp" if instruction[0] == "nop" else "nop"
            # process
            success,acc = execute(data_copy)
            if success:
                print("Success; changing line {} yields {} ({})".format(i,acc,instruction))
                break
