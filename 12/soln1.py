import csv
import copy
import math

def traverse(path, start):
    """ Traverse the given path, giving the final position """
    current = copy.copy(start)

    # do stuff
    for action,dist in path:
        if action == "N":
            current[1] += dist
        elif action == "S":
            current[1] -= dist
        elif action == "E":
            current[0] += dist
        elif action == "W":
            current[0] -= dist
        elif action == "L":
            current[2] = (current[2] + dist) % 360
        elif action == "R":
            current[2] = (current[2] - dist) % 360
        elif action == "F":
            if current[2] == 0:
                # east
                current[0] += dist
            elif current[2] == 90:
                # north
                current[1] += dist
            elif current[2] == 180:
                # west
                current[0] -= dist
            elif current[2] == 270:
                # south
                current[1] -= dist
            else:
                import pdb;pdb.set_trace()
        else:
            import pdb;pdb.set_trace()

    return current


if __name__ == "__main__":
    # load data
    data = []
    with open("input1.txt", "r") as csvfile:
        reader = csv.reader(csvfile)
        for row in reader:
            if (len(row) != 0):
                data.append([row[0][0], int(row[0][1:])])

    # part #1, get how far we've traveled
    start = [0.0, 0.0, 0.0]
    end = traverse(data, start)

    print(abs(end[0]) + abs(end[1]))
