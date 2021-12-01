import csv
import copy

def game(p1, p2):
    """ Play until a player wins

    Returns True if p1 wins, else False
    Also returns the winning player's deck
    """

    print("New game: \n")

    previous_rounds = []
    count = 0
    while True:
        count += 1
        print("\tRound #{}".format(count))

        print("\tPlayer 1's deck: {}".format(p1))
        print("\tPlayer 2's deck: {}".format(p2))

        # check stop conditions
        if (hash(str([p1,p2]))) in previous_rounds:
            # player 1 wins automatically
            print("Automatic win for P1")
            return True,p1

        # add this round to the list or rounds
        previous_rounds.append(hash(str([p1,p2])))

        # draw cards
        c1 = p1.pop(0)
        c2 = p2.pop(0)

        print("\tPlayer 1 plays: {}".format(c1))
        print("\tPlayer 2 plays: {}".format(c2))

        # check if we should recurse
        p1_winner = None
        if (len(p1) >= c1) and (len(p2) >= c2):
            p1_winner,_ = game(copy.deepcopy(p1[:c1]),copy.deepcopy(p2[:c2]))
            print("Finished subgame \n")
        else:
            p1_winner = (c1 > c2)

        # process winning results
        if p1_winner:
            print("\tPlayer 1 wins")
            p1 += [c1,c2]
        else:
            print("\tPlayer 2 wins")
            p2 += [c2,c1]

        # check for an empty deck
        if len(p1) == 0:
            return False,p2
        elif len(p2) == 0:
            return True,p1

if __name__ == "__main__":
    # load data
    p1 = []
    p2 = []
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
    _,winner = game(p1,p2)

    score = 0
    for i,c in enumerate(winner):
        score += c*(len(winner)-i)
    print("Winning deck: {}".format(winner))
    print("Winning score: {}".format(score))
