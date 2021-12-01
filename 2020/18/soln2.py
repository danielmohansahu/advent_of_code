import csv
import copy
import numpy as np

def eval_expr(expr):
    """ Evaluate an expression without parantheses.
    """
    current = "".join(expr)
    result = 1
    for subexpr in current.split("*"):
        result *= eval(subexpr) 
    return result

def _calc(expr):
    """ Recursive calc to remove parantheses.
    """
    print("Processing: {}".format("".join(expr)))

    # check stop conditions
    latest = copy.deepcopy(expr)
    while "(" in latest:
        # find and replace a nested subexpression
        st_idx = len(latest)-latest[::-1].index("(")
        en_idx = latest[st_idx:].index(")") + st_idx
        latest[st_idx-1:en_idx+1] = [str(eval_expr(latest[st_idx:en_idx]))]
        print("\t now: {}".format("".join(latest))) 
    return eval_expr(latest)

def calc(line):
    total = _calc(line)
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
