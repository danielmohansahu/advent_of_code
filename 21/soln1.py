import csv
from collections import defaultdict
import copy

if __name__ == "__main__":
    # load data
    allergen_options = defaultdict(list)
    all_ingredients = []  
    with open("input.txt", "r") as csvfile:
        reader = csv.reader(csvfile, delimiter='\n')
        # first read in rules
        for row in reader:
            if len(row) != 0:
                # split and clean up
                ingredients,allergens = row[0].split("(contains")
                ingredients = ingredients.split()
                allergens = allergens.replace(")","").split(",")
                for a in allergens:
                    allergen_options[a.strip()].append(set(ingredients))
                all_ingredients += ingredients

    # part 1, find simple allergens
    # go through each of the allergens and determine the subset of ingredients it must belong to
    potentials = set()
    for allergen, opts in allergen_options.items():
        potentials = potentials.union(set.intersection(*opts))
    safe = set(all_ingredients) - potentials

    # go through the full list again to get a count of each potential ingredient
    count = 0
    for ingredient in safe:
        count += all_ingredients.count(ingredient)
    print("Safe foods occurred {} times: {}".format(count,safe))

    # part #2: print out our danger list
    dangerous = {}
    iteration = 0
    # iteratively solve
    while len(dangerous) != len(allergen_options):
        iteration += 1
        for allergen, opts in allergen_options.items():
            # check if we've already solved this
            if allergen in dangerous:
                continue

            # get the exclusive list of remaining potentials
            options = set.intersection(*opts) - set(dangerous.values())
            if len(options) == 1:
                dangerous[allergen] = options.pop()
        print("Iteration {}; dangerous: {}".format(iteration,dangerous))
    
    # format
    sorted_ingredients = [t for t in zip(dangerous.keys(),dangerous.values())]
    sorted_ingredients.sort()
    result = ",".join([i for _,i in sorted_ingredients])
    print("Canonical dangerous list:\n{}".format(result))


