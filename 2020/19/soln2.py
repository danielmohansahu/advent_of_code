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
    print("Results for rule {}: {}".format(rule, results))
    return results

def expand(rules):
    # hackily get 8,11
    solved = {}
    try:
        result = _eval_rule(rules,8,solved)
    except RecursionError:
        pass
    try:
        result = _eval_rule(rules,11,solved)
    except RecursionError:
        pass
    return solved

if __name__ == "__main__":
    # load data
    rules = {}
    data = []
    with open("input2.txt", "r") as csvfile:
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

    # part 2, handle infinite recursion via hardcoding
    solved = expand(rules)
    
    # check assumptions
    assert(len(solved[31][0]) == len(solved[42][0]))
    length = len(solved[31][0])

    count = 0
    for datum in data:
        # check if this matches our rule 8 (sequences of 42) 
        #  or our rule 11 (sequences of 42 followed by sequences of 31)

        # quick check; must be a multiple of length
        if len(datum) % length != 0:
            continue

        # convert to a list of 42s and 31 for simpler analysis
        equi = []
        i = 0
        success = True
        while length*i < len(datum):
            substr = datum[length*i:length*(i+1)]
            if substr in solved[42]:
                equi.append(42)
            elif substr in solved[31]:
                equi.append(31)
            else:
                success = False
                break
            i+=1

        # stop if this is already bad
        if not success:
            continue

        # start must be 42, end must be 31
        if (equi[0] != 42) or (equi[-1] != 31):
            continue

        # at this point all we need to check is that the final elements
        #  are all 31, and there are fewer (or equal) 31 than 42
        if (equi[equi.index(31):].count(42) == 0) and (equi.count(31) < equi.count(42)):
            print(equi)
            count += 1

    import pdb;pdb.set_trace()
    print("Found {} good messages.".format(count))
