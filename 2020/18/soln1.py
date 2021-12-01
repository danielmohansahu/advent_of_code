import csv
import numpy as np

def _calc(expr, total, op):
    """ Recursive calc.
    """
    print("\tProcessing: {}".format("".join(expr)))
    i = 0
    while i != len(expr):
        e = expr[i]
        print("\t\ttotal, op, i, e: {},{},{},{}".format(total,op,i,e))
        if e.isnumeric():
            if total is None:
                # first number
                total = int(e)
            else:
                # apply this number
                assert(op is not None)
                if op == "+":
                    total += int(e)
                else:
                    total *= int(e)
        elif e in ["+", "*"]:
            # update our operator
            op = e
        elif e == "(":
            # push down into a new scope
            subtotal,skip = _calc(expr[i+1:],None,None)
            if op is None:
                total = subtotal
            elif op == "+":
                total += subtotal
            else:
                total *= subtotal
            i += skip + 1
        else:
            # stop condition
            assert(e == ")")
            return total,i
        i += 1
    # if we've gotten this far it's the end of the expression, return
    return total,i

def calc(line):
    total = _calc(line, None, None)[0]
    print("Total: {}".format(total))
    return total

if __name__ == "__main__":
    # load data
    data = []
    with open("input.txt", "r") as csvfile:
        reader = csv.reader(csvfile)
        # first read in rules
        for row in reader:
            if len(row) != 0:
                expression = []
                for element in row[0].split():
                    expression += [e for e in element]
                data.append(expression)

    # part 1, sum each line
    sum_ = 0
    for line in data:
        sum_ += calc(line)
    print("Part #1 sum: {}".format(sum_))
