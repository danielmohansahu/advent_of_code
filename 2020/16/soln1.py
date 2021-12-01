import csv
import copy
import math
from collections import defaultdict

if __name__ == "__main__":
    # load data
    my_ticket = None
    tickets = []
    rules = {}
    with open("input.txt", "r") as csvfile:
        reader = csv.reader(csvfile)
        # first read in rules
        for row in reader:
            if len(row) == 0:
                break
            info = row[0].split(":")
            rules[info[0]] = [tuple([int(v) for v in val.split("-")]) for val in info[1].split("or")]
        print("Rules: {}".format(rules))

        # then read in my ticket
        next(reader)
        my_ticket = [int(r) for r in next(reader)]
        print("My ticket: {}".format(my_ticket))

        # skip blank/useless lines, then record other tickets
        next(reader)
        next(reader)
        for row in reader:
            if len(row) != 0:
                tickets.append([int(r) for r in row])
        print("Other tickets: {}".format(tickets))


    # part #1: find invalid tickets

    # get full set of allowed values
    valid = set().union(*[set().union(*[range(b,t+1) for b,t in val]) for val in rules.values()])
    
    # find invalid tickets
    invalid = []
    valid_tickets = []
    for ticket in tickets:
        remainder = set(ticket) - valid
        if len(remainder) != 0:
            # print("Found invalid ticket field {}".format(*remainder))
            invalid.append(*remainder)
        else:
            valid_tickets.append(ticket)
    print("Sum: {}".format(sum(invalid)))

    # part #2: Determine which fields belong where

    # recompute valid sets 
    valid = {k:set().union(*[range(b,t+1) for b,t in v]) for k,v in rules.items()}

    # for key,val in valid.items():
    #     others = set().union(*[set().union(*[range(b,t+1) for b,t in v]) for k,v in rules.items() if k!=key])
    #     if len(val - others) != 0:
    #         import pdb;pdb.set_trace()
    # print("Nothing found...")
    # import pdb;pdb.set_trace()

    # iterate through patterns, trying to find exclusive fields
    solved = {}
    while len(solved) != len(my_ticket):
        potentials = {}
        for ticket in valid_tickets:
            for i,field in enumerate(ticket):
                # skip solved fields
                if i not in solved:
                    # check if this field is an exclusive field
                    within = set([k for k,v in valid.items() if ((k not in solved.values()) and (field in v))]) - set(solved.values())

                    # sanity check
                    assert(len(within) != 0)

                    # add it to the list of potentials
                    if i not in potentials:
                        potentials[i] = within
                    else:
                        potentials[i] = potentials[i].intersection(within)

                    # check if this field is now "solved"
                    if len(within) == 1:
                        assert(i not in solved)
                        solved[i] = within.pop()
                    elif len(potentials[i]) == 1:
                        assert(i not in solved)
                        solved[i] = potentials[i].pop()
        # check if any of the potentials are the only ones with a give key
        for field in valid.keys():
            # skip already solved ones
            if field not in solved.values():
                # how many potentials actually contain this field?
                options = [k for k,p in potentials.items() if field in p]
                if len(options) == 1:
                    print("Field {} can only be in {}".format(field,options[0]))
                    assert(options[0] not in solved)
                    solved[options[0]] = field

        # status report
        print("\titeration with {}/{} solved".format(len(solved), len(my_ticket)))
            
    # print result for our ticket
    result = 1
    for k,v in solved.items():
        if "departure" in v:
            result *= my_ticket[k]
    print("Departure multiple: {}".format(result))

    

    
            
