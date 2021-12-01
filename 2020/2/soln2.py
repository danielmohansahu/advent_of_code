import csv

if __name__ == "__main__":
    # load data (same as part #1)
    data = []
    with open("input1.txt", "r") as csvfile:
       reader = csv.reader(csvfile)
       for row in reader:
           if len(row) != 0:
               data.append(row[0].split())

    # go through data (list of lists of conditions / passwords)
    valid = 0
    for cond, letter, password in data:
        # convert indices to index from 0
        idx1,idx2 = [int(c)-1 for c in cond.split("-")]
        # get rid of the ':'
        l = letter[0]
        # check if only one index has the correct password
        if (password[idx1] == l) ^ (password[idx2] == l):
            valid += 1
    print(valid)

