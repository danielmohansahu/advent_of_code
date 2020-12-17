import csv
from collections import defaultdict

if __name__ == "__main__":
    # load data
    data = {}
    with open("input1.txt", "r") as csvfile:
        reader = csv.reader(csvfile, delimiter='\n')
        for row in reader:
            if (len(row) != 0):
                # hardcoded heuristics: 
                #  first two words are color
                #  split on keyword 'contain'
                #  final fragment is either:
                #    empty ('no other bags.')
                #    comma separated list of N sub-colors
                words = row[0].split()
                color = " ".join(words[:2])
                fragment = " ".join(words[4:])
                subbags = {}
                if fragment != "no other bags.":
                    for sbag in fragment.split(","):
                        sbag_words = sbag.split()
                        subbags[" ".join(sbag_words[1:3])] = int(sbag_words[0])
                data[color] = subbags

    # now we have our graph; we can do a breadth first search to find solutions

    # for each bag type, dive until either we find it contains a 'shiny gold' bag or
    #  we exhaust all possibilities
    goal = "shiny gold"
    count = 0
    for bag, subbags in data.items():
        # check edge cases
        if bag == goal:
            continue
        else:
            # do a sub-search for all the sub-bags
            current = subbags.keys()
            found = False
            while len(current) != 0:
                new = []
                for sbag in current:
                    if sbag == goal:
                        found = True
                        break
                    else:
                        new += data[sbag].keys()

                # check stop condition
                if found:
                    count += 1
                    break
                current = new

    print("{} different bags can contain '{}' bag".format(count,goal))  

    # part 2 is simpler; just find the number of bags required inside a shiny gold bag
    bag_count = 0
    current = data[goal]
    while len(current) != 0:
        # print("Starting from {}".format(current))
        new = defaultdict(int)
        # iterate through lower bags
        for subbag, cost in current.items():
            # add this bags' cost
            bag_count += cost
            # add this bags sub-bags to the new list
            for k,v in data[subbag].items():
                new[k] += cost * v
        # print("Updating to {}".format(new))
        current = new
    print("A '{}' bag must contain {} other bags".format(goal,bag_count))









