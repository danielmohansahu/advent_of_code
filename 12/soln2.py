import csv
import copy
import math

def traverse(path, start, waypoint):
    """ Traverse the given path, giving the final position """
    current = copy.copy(start)

    # do stuff
    for action,dist in path:
        if action == "N":
            waypoint[1] += dist
        elif action == "S":
            waypoint[1] -= dist
        elif action == "E":
            waypoint[0] += dist
        elif action == "W":
            waypoint[0] -= dist
        elif action in ["L","R"]:
            angle = (1 if (action == "L") else -1) * dist * math.pi/180
            x = math.cos(angle) * waypoint[0] - math.sin(angle) * waypoint[1]
            y = math.sin(angle) * waypoint[0] + math.cos(angle) * waypoint[1]
            waypoint[0] = x
            waypoint[1] = y
        elif action == "F":
            current[0] += waypoint[0] * dist
            current[1] += waypoint[1] * dist
        # print(waypoint)
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
    end = traverse(data, start, [10,1])

    print(abs(end[0]) + abs(end[1]))
