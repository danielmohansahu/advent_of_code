import csv
import copy

def split(arrays, branches):
    """ Split the given arrays into more arrays, 
    each with a branch appended.
    """
    result = []
    for array in arrays:
        for branch in branches:
            result.append(array + branch)
    return result


def _eval_rule(rules, rule, solved):
    # check if we've already solved this one
    if rule in solved:
        return solved[rule]

    # initialize output
    results = []
    
    # each subrule represents an OR statement
    for subrule in rules[rule]:
        options = [""]
        # now we break it down into AND statements
        for seq in subrule.split():
            # check if it's a string (terminal) or a nested rule
            if seq.isnumeric():
                # recursively evaluate
                res = _eval_rule(rules, int(seq), solved)
                # note that multiple results here indicate more branches
                # for our result sequence
                options = split(options, res)
            else:
                # this is a stop condition, great
                options = split(options, [seq])
        results += options

    # save 
    solved[rule] = results
    # print("Results for rule {}: {}".format(rule, results))
    return results

def expand(rules):
    solved = {}
    return _eval_rule(rules,0,solved)

if __name__ == "__main__":
    # load data
    rules = {}
    data = []
    with open("input.txt", "r") as csvfile:
        reader = csv.reader(csvfile)
        # first read in rules
        for row in reader:
            if len(row) == 0:
                break
            expr = row[0].split(":")
            rules[int(expr[0])] = [e.strip().replace('"','') for e in expr[1].split("|")]
        # then read in the inputs
        for row in reader:
            if len(row) != 0:
                data.append(row[0])
    print(rules)

    # part 1, sum each line
    opts = expand(rules)
    
    count = 0
    for datum in data:
        if datum in opts:
            count += 1
    print("Found {} good messages.".format(count))
