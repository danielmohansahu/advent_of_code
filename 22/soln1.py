import csv
import copy


def play(p1, p2):
    """ Play until a player wins
    """
    while (len(p1) != 0) and (len(p2) != 0):
        c1 = p1.pop(0)
        c2 = p2.pop(0)
        if c1 > c2:
            p1 += [c1,c2]
        else:
            p2 += [c2,c1]

    # return the winning remainder
    if len(p1) != 0:
        return p1
    else:
        return p2


if __name__ == "__main__":
    # load data
    p1 = []
    p2 = []
    player = None
    with open("input.txt", "r") as csvfile:
        reader = csv.reader(csvfile)

        # process player 1
        next(reader)
        for row in reader:
            if len(row) == 0:
                break
            p1.append(int(row[0]))

        # process player 2
        next(reader)
        for row in reader:
            if len(row) == 0:
                break
            p2.append(int(row[0]))
   
    # print
    print("P1: {}".format(p1))
    print("P2: {}".format(p2))

    # part #1: play a round
    winner = play(p1,p2)

    score = 0
    for i,c in enumerate(winner):
        score += c*(len(winner)-i)
    print("Winning score: {}".format(score))
