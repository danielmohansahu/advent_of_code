import csv

if __name__ == "__main__":
    # load data
    data = []
    with open("input1.txt", "r") as csvfile:
        reader = csv.reader(csvfile)
        group = []
        for row in reader:
            # check if this is a new group
            if len(row) == 0:
                data.append(group)
                group = []
            else:
                group.append(row[0])

    # part #1: get the count for part 1
    # anyone answered "yes"
    count = 0
    for group in data:
        group_answer = set(group[0]).union(*[set(member) for member in group[1:]])
        count += len(group_answer)
    print(count)

    # part #1: get the count for part 2
    # aeveryone answered "yes"
    count = 0
    for group in data:
        group_answer = set(group[0]).intersection(*[set(member) for member in group[1:]])
        count += len(group_answer)
    print(count)

